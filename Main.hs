module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Map.Strict as M (Map, empty, insertWith, foldl', delete, unionWith, adjust)
import Data.Text as T (Text, pack, unlines)
import Gramma.Par (pProgram, myLexer)
import MyFuns.Numbers
import MyFuns.Flow
import Gramma.Abs
import Debug.Trace
import MyFuns.SimpleLanguage
import MyFuns.Values
import Data.List (genericLength)
import Control.Exception (catch, ErrorCall(..))
import Data.Semigroup (Max(..))
import Control.Monad.State.Strict (State, get, put, state, runState, evalState)

generateFastFor :: Pidentifier -> Value -> Value -> [Command] -> Bool -> State SymbolTable [OpCode]
generateFastFor pid@(Pidentifier (_, txt)) from to body up = do
  st <- get
  let st' = safeInsert txt (IterInfo (getFreeMem st) True) st
  let limit = LimitId pid
  let iter = ScalarId pid
  let step = (if up then Plus else Minus) (IdValue (ScalarId pid)) (NumValue (Number (T.pack "1")))
  let (codeB, st'') = runState (generateCommands body) st'
  put $ M.delete txt st''
  return $ generateIter st' st limit (ValueExpr to) ++ 
    getValue st F from  ++
    generateWhile (genCond st' ((if up then Leq else Geq) (IdValue iter) (IdValue limit)))
    (codeB ++ (if up then [INC F] else [DEC F]))

generateCommands :: [Command] -> State SymbolTable [OpCode]
generateCommands = fmap concat . mapM generateCommand

generateIter :: SymbolTable -> SymbolTable -> Identifier -> Expression -> [OpCode]
generateIter st1 st2 id exp =
  case getIdAddr False st1 B id of
    Just codeB ->
      codeB ++ getExpression st2 exp ++ [STORE B]
    Nothing ->
      error "generateIter internal error"

generateFor :: Pidentifier -> Value -> Value -> [Command] -> Bool -> State SymbolTable [OpCode]
generateFor pid@(Pidentifier (_, txt)) from to body up = do
  st <- get
  let st' = safeInsert txt (IterInfo  (getFreeMem st) False) st
  let iter = ScalarId pid
  let limit = LimitId pid
  let step = (if up then Plus else Minus) (IdValue (ScalarId pid)) (NumValue (Number (T.pack "1")))
  let (codeB, st'') = runState (generateCommands body) st'
  put $ M.delete txt st''
  return $ generateIter st' st iter (ValueExpr from) ++
    generateIter st' st limit (ValueExpr to) ++
    generateWhile (genCond st' ((if up then Leq else Geq) (IdValue iter) (IdValue limit))) (
    codeB ++ generateIter st' st' iter step)

combineSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
combineSymbolTables = M.unionWith combineVarInfo

combineVarInfo :: VarInfo -> VarInfo -> VarInfo
combineVarInfo (ScalarInfo addr1 init1) (ScalarInfo addr2 init2) =
  if addr1 == addr2 then
    ScalarInfo addr1 $ init1 || init2
  else
    error "internal error"
combineVarInfo vi1 vi2 =
  if vi1 == vi2 then vi1 else error "internal error"

initialize :: SymbolTable -> Identifier -> SymbolTable
initialize st (ScalarId (Pidentifier (_, txt))) = M.adjust adjuster txt st
  where
    adjuster (ScalarInfo addr _) = ScalarInfo addr True
    adjuster info = info
initialize st id = st

generateCommand :: Command -> State SymbolTable [OpCode]
generateCommand (Assign id exp) =
  state (\st -> ( 
    case getIdAddr True st B id of
      Just codeB ->
        (codeB ++ getExpression st exp ++ [STORE B], initialize st id)
      Nothing ->
        error "internal error"))
generateCommand (IfElse cond cmdsI cmdsE) = do
  st <- get
  let (codeI, stI) = runState (generateCommands cmdsI) st
  let lenI = genericLength codeI
  let (codeE, stE) = runState (generateCommands cmdsE) st
  let lenE = genericLength codeE
  put (combineSymbolTables stI stE)
  return (genCond st cond (lenI + 1) ++ codeI ++ [JUMP (CodePos (lenE + 1))] ++ codeE)
generateCommand (While cond body) = do
  st <- get
  codeB <- generateCommands body
  return (generateWhile (genCond st cond) codeB)
generateCommand (Repeat body cond) = do
  codeB <- generateCommands body
  let lenB = genericLength codeB
  st <- get
  let
    codeC = genCond st cond (negate (lenB + lenC - 1))
    lenC = genericLength codeC
  return (codeB ++ codeC)
generateCommand (ForTo pid from to body) 
  | x == False = generateFor pid from to body True
  | x == True = generateFastFor pid from to body True
  where x = fastFor body
generateCommand (ForDownTo pid from to body)
  | x == False = generateFor pid from to body False
  | x ==True = generateFastFor pid from to body False
  where x = fastFor body
generateCommand (Read id) =
  state (\st -> (
    case getIdAddr True st B id of 
      Just codeB ->
        (codeB ++ [GET, STORE B], initialize st id)
      Nothing ->
        error "internal error"))
generateCommand (Write x) = do
  st <- get
  return (getValue st B x ++ [SWAP B, PUT])

getFreeMem :: SymbolTable -> Integer
getFreeMem st = M.foldl' (\a x -> getAddress x `max` a) 0 st
  where
    getAddress (ScalarInfo a _) = a + 1
    getAddress (IterInfo a b) = a + 2
    getAddress (ArrayInfo a b e) = a + e - b + 1

generateSymbolTable :: Integer -> [Declaration] -> SymbolTable
generateSymbolTable _ [] = M.empty
generateSymbolTable freeMem (ScalarDecl (Pidentifier (_, txt)) : xs) =
  safeInsert txt (ScalarInfo freeMem False) $
    generateSymbolTable (freeMem + 1) xs
generateSymbolTable freeMem (ArrayDecl (Pidentifier (_, txt)) b e : xs) =
  safeInsert txt (ArrayInfo freeMem b' e') $
    generateSymbolTable (freeMem + e' - b' + 1) xs
      where
        e' = valueOf e
        b' = valueOf b

safeInsert :: Text -> VarInfo -> SymbolTable -> SymbolTable
safeInsert = M.insertWith (\ _ _ -> error "redeclared variable")

generateCode :: Program -> [OpCode]
generateCode (Program declarations commands) =
  evalState (generateCommands commands) (generateSymbolTable 0 declarations)

showText :: Show a => a -> Text
showText = pack . show

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath] -> do
      inputText <- TIO.readFile inputPath
      let inputTokens = myLexer inputText
      case pProgram inputTokens of
        Left errorMessage -> putStrLn errorMessage
        Right abstractSyntaxTree -> do
          let generatedCode = generateCode abstractSyntaxTree ++ [HALT]
          catch (TIO.writeFile outputPath $ T.unlines $ showText <$> generatedCode) $
            \(ErrorCall e) -> putStrLn e
