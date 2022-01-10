module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Map as M (Map, empty, insert)
import Data.Text as T (Text, pack, unlines)
import Gramma.Par (pProgram, myLexer)
import MyFuns.Numbers (VarInfo(..), SymbolTable, valueOf, getExpression, getValue, getIdAddr)
import MyFuns.Flow (genCond)
import Gramma.Abs
import Debug.Trace
import MyFuns.SimpleLanguage
import Data.List (genericLength)
import Control.Exception (catch, ErrorCall(..))

optimize :: Program -> Program
optimize = id

analize:: Program -> Program
analize = id

generateCommands :: SymbolTable -> [Command] -> [OpCode]
generateCommands = concatMap . generateCommand

generateCommand :: SymbolTable -> Command -> [OpCode]
generateCommand st (Assign id exp) =
  getIdAddr st B id ++ getExpression st exp ++ [SWAP C, STORE B]
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
generateCommand st (Read id) = getIdAddr st B id ++ [GET, STORE B]
generateCommand st (Write x) = getValue st B x ++ [SWAP B, PUT]

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
