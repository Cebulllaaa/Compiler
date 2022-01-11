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

generateIter :: SymbolTable -> SymbolTable -> Identifier -> Expression -> [OpCode]
generateIter st1 st2 id exp =
  getIdAddr False st1 B id ++ getExpression st2 exp ++ [STORE B]

generateWhile :: (Integer -> [OpCode]) -> [OpCode] -> [OpCode]
generateWhile codeC codeB =
  codeC' ++ codeB ++ [JUMP (CodePos (negate (lenC + lenB)))]
    where
      codeC' = codeC (lenB + 1)
      lenC = genericLength codeC'
      lenB = genericLength codeB

generateFor :: SymbolTable -> Pidentifier -> Value -> Value -> [Command] -> Bool -> [OpCode]
generateFor st pid from to body up =
  generateIter st' st iter (ValueExpr from) ++
  generateIter st' st limit (ValueExpr to) ++
  generateWhile (genCond st' ((if up then Leq else Geq) (IdValue iter) (IdValue limit))) (
    generateCommands st' body ++ generateIter st' st' iter step)
  where
    st' = M.insert pid (IterInfo (getFreeMem st)) st
    iter = ScalarId pid
    limit = LimitId pid
    step = (if up then Plus else Minus) (IdValue (ScalarId pid)) (NumValue (Number (T.pack "1")))


generateCommand :: SymbolTable -> Command -> [OpCode]
generateCommand st (Assign id exp) =
  getIdAddr True st B id ++ getExpression st exp ++ [STORE B]
generateCommand st (IfElse cond cmdsI cmdsE) =
  genCond st cond (lenI + 1) ++ codeI ++ [JUMP (CodePos (lenE + 1))] ++ codeE
    where
      codeI = generateCommands st cmdsI
      lenI = genericLength codeI
      codeE = generateCommands st cmdsE
      lenE = genericLength codeE
generateCommand st (While cond body) =
  generateWhile (genCond st cond) (generateCommands st body)
generateCommand st (Repeat body cond) =
  codeB ++ codeC
    where
      codeB = generateCommands st body
      lenB = genericLength codeB
      codeC = genCond st cond (negate (lenB + lenC - 1))
      lenC = genericLength codeC
generateCommand st (ForTo pid from to body) =
  generateFor st pid from to body True
generateCommand st (ForDownTo pid from to body) =
  generateFor st pid from to body False
generateCommand st (Read id) = getIdAddr True st B id ++ [GET, STORE B]
generateCommand st (Write x) = getValue st B x ++ [SWAP B, PUT]

getFreeMem :: SymbolTable -> Integer
getFreeMem st = M.foldl' (\a x -> getAddress x `max` a) 0 st
  where
    getAddress (ScalarInfo a) = a + 1
    getAddress (IterInfo a) = a + 2
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
