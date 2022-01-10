module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Map as M (Map, empty, insert)
import Data.Text as T (Text, pack, unlines)
import Gramma.Par (pProgram, myLexer)
import MyFuns.Numbers (generateIncCode, valueOf, getExpression, getValue, getIdAddr)
import MyFuns.Flow (genCond, genUntilCode)
import Gramma.Abs
import Debug.Trace
import MyFuns.SimpleLanguage
import Data.List (genericLength)

optimize :: Program -> Program
optimize = id

analize:: Program -> Program
analize = id

generateCommands :: SymbolTable -> [Command] -> [OpCode]
generateCommands = concatMap . generateCommand

generateCommand :: SymbolTable -> Command -> [OpCode]
generateCommand st (Write x) =
  getValue A x ++ [PUT]
generateCommand st (Write (IdValue x)) = [RESET A] ++ [SWAP D]  ++[PUT]
generateCommand st (IfElse cond cmdsI cmdsE) =
  genCond cond (CodePos (genericLength blockIf + 1)) ++
    blockIf ++ generateCommands st cmdsE
  where
    blockIf = generateCommands st cmdsI
generateCommand st (Assign id exp) = getIdAddr B id ++ getExpression exp ++ [STORE B]
generateCommand st (Read id) = []
generateCommand st (Repeat cmds cond) = body ++ genUntilCode cond (genericLength body)
  where
    body = concatMap (generateCommand st) cmds

data VarInfo
  = ScalarInfo {address :: Integer}
  | ArrayInfo {address :: Integer, begin :: Integer, end :: Integer}

type SymbolTable = M.Map Pidentifier VarInfo

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
          TIO.writeFile outputPath $ T.unlines $ showText <$> generatedCode
