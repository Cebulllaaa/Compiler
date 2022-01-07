module Main where

import System.Environment (getArgs)
import Data.Text.IO as TIO (readFile, writeFile)
import Data.Text as T
import Data.Sequence
import Gramma.Par (pProgram, myLexer)
import MyFuns.Numbers (generateIncCode, valueOf, getExp)
import MyFuns.Flow (genCond)
import Gramma.Abs 
import Debug.Trace
import MyFuns.SimpleLanguage


corText :: Text -> Text 
corText t = 
    T.map (\c -> if c ==',' then '\n'  else c ) $ T.filter (not . (`elem` "[]")) t

optimize :: Program -> Program
optimize = id

analize:: Program -> Program
analize = id

generateOpCode :: Command -> [OpCode]
generateOpCode (Write ( NumValue x))  = 
    [RESET H] ++ (generateIncCode ((valueOf x ) -1) [INC H] H) ++ [LOAD H] ++ [PUT]
generateOpCode (IfElse cond cmdsI cmdsE) = 
    (genCond cond ((Prelude.length $ Prelude.concat (Prelude.map  generateOpCode  cmdsI)) + 1))  
    ++  Prelude.concat (Prelude.map  generateOpCode  cmdsI) ++ Prelude.concat (Prelude.map  generateOpCode  cmdsE)
generateOpCode ( IfElseSkip  cond cmds ) = 
    (genCond cond ((Prelude.length $ Prelude.concat (Prelude.map  generateOpCode  cmds)) + 1))  ++  Prelude.concat (Prelude.map  generateOpCode  cmds)
generateOpCode (Assign id exp) = getExp exp


generateCode :: Program -> [[OpCode]]
generateCode (Program declarations commands)  = 
    Prelude.map generateOpCode commands

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
                    let generatedCode = (generateCode optimizedTree) ++ [[HALT]]
                
                    TIO.writeFile outputPath $ corText $ T.unlines  $ showText <$> generatedCode
