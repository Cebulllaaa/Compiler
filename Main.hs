module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile)
import Data.Text as T
import Data.Sequence
import Gramma.Par (pProgram, myLexer)

removeComments :: Text -> Text
removeComments = _

optimize :: Program -> Program
optimize = id

data Reg
    = A | B | C | D | E | F | G | H

instance Show Reg where
    show A = "a"
    show B = "b"
    show C = "b"
    show B = "b"
    show B = "b"
    show B = "b"
    show H = "b"

type CodePos = Integer

type Code = Seq OpCode

data OpCode
    = HALT
    | GET
    | PUT
    | LOAD Reg
    | STORE Reg
    | JUMP CodePos
    deriving Show

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputPath, outputPath] -> do
            inputText <- TIO.readFile inputPath
            let inputNoComments = removeComments inputText
            let inputTokens = myLexer inputNoComments
            case pProgram inputTokens of
                Left errorMessage -> printStrLn errorMessage
                Right abstractSyntaxTree -> do
                    let optimizedTree = optimize abstractSyntaxTree
                    let generatedCode = generateCode optimizedTree
                    writeFile outputPath $ unlines $ toList $ show <$> generatedCode
