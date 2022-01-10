module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Map as M (Map, empty, insert, foldl')
import Data.Text as T (Text, pack, unlines)
import Gramma.Par (pProgram, myLexer)
import MyFuns.Numbers (VarInfo(..), SymbolTable, valueOf, getExpression, getValue, getIdAddr)
import MyFuns.Flow (genCond)
import Gramma.Abs
import Debug.Trace
import MyFuns.SimpleLanguage
import Data.List (genericLength)
import Control.Exception (catch, ErrorCall(..))
import Data.Semigroup (Max(..))

optimize :: Program -> Program
optimize = id

analize:: Program -> Program
analize = id

generateCommands :: SymbolTable -> [Command] -> [OpCode]
generateCommands = concatMap . generateCommand

generateCommand :: SymbolTable -> Command -> [OpCode]
generateCommand st (Iter id exp) =
  getIdAddr False st B id ++ getExpression st exp ++ [SWAP C, STORE B]
generateCommand st (Assign id exp) =
  getIdAddr True st B id ++ getExpression st exp ++ [SWAP C, STORE B]
generateCommand st (IfElse cond cmdsI cmdsE) =
  genCond st cond (\_ -> generateCommands st cmdsI) (\_ -> generateCommands st cmdsE)
generateCommand st (While cond body) =
  genCond st cond (\_ -> generateCommands st body) (\offset -> [JUMP (CodePos (negate offset))])
generateCommand st (Repeat body cond) =
  bodyCode ++ genCond st cond (\_ -> []) (\o -> [JUMP (CodePos (start o))])
  where
    bodyCode = generateCommands st body
    bodyLen = genericLength bodyCode
    start offset = negate offset - bodyLen
generateCommand st (ForTo pid from to body) =
  generateCommands st' [
    Iter (ScalarId pid) (ValueExpr from),
    While (Leq (IdValue (ScalarId pid)) to) $
      body ++
        [Iter (ScalarId pid) (Plus (IdValue (ScalarId pid)) (NumValue (Number (T.pack "1"))))]
  ]
  where
    st' = M.insert pid (IterInfo (getFreeMem st)) st
generateCommand st (ForDownTo pid from to body) =
  generateCommands st' [
    Iter (ScalarId pid) (ValueExpr from),
    While (Geq (IdValue (ScalarId pid)) to) $
      body ++
        [Iter (ScalarId pid) (Minus (IdValue (ScalarId pid)) (NumValue (Number (T.pack "1"))))]
  ]
  where
    st' = M.insert pid (IterInfo (getFreeMem st)) st
generateCommand st (Read id) = getIdAddr True st B id ++ [GET, STORE B]
generateCommand st (Write x) = getValue st B x ++ [SWAP B, PUT]

getFreeMem :: SymbolTable -> Integer
getFreeMem st = M.foldl' (\a x -> getAddress x `max` a) 0 st
  where
    getAddress (ScalarInfo a) = a + 1
    getAddress (IterInfo a) = a + 1
    getAddress (ArrayInfo a b e) = a + e - b + 1

generateSymbolTable :: Integer -> [Declaration] -> SymbolTable
generateSymbolTable _ [] = M.empty
generateSymbolTable freeMem (ScalarDecl pid : xs) =
  M.insert pid (ScalarInfo freeMem) $
    generateSymbolTable (freeMem + 1) xs
generateSymbolTable freeMem (ArrayDecl pid b e : xs) =
  M.insert pid (ArrayInfo freeMem b' e') $
    generateSymbolTable (freeMem + e' - b' + 1) xs
      where
        e' = valueOf e
        b' = valueOf b

generateCode :: Program -> [OpCode]
generateCode (Program declarations commands) =
  concatMap (generateCommand (generateSymbolTable 0 declarations)) commands

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
          let analizedTree = analize abstractSyntaxTree
          let optimizedTree = optimize analizedTree
          let generatedCode = generateCode optimizedTree ++ [HALT]
          catch (TIO.writeFile outputPath $ T.unlines $ showText <$> generatedCode) $
            \(ErrorCall e) -> putStrLn e
