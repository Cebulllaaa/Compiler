module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import Data.Text as T
import Data.Sequence
import Gramma.Par (pProgram, myLexer)
import Gramma.Abs 

optimize :: Program -> Program
optimize = id

analize:: Program -> Program
analize = id

generateOpCode :: Command -> OpCode
generateOpCode (Write x ) = 

generateCode :: Program -> [OpCode]
generateCode (Program declarations commands) = undefined

showText :: Show a => a -> Text
showText = pack . show


data Reg = 
    A | B | C | D | E | F | G | H deriving  Show

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
            let inputTokens = myLexer inputText
            case pProgram inputTokens of
                Left errorMessage -> putStrLn errorMessage
                Right abstractSyntaxTree -> do
                    let analizedTree = analize abstractSyntaxTree
                    let optimizedTree = optimize analizedTree
                    let generatedCode = generateCode optimizedTree
                    TIO.writeFile outputPath $ T.unlines  $ showText <$> generatedCode
