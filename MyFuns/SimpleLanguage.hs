module MyFuns.SimpleLanguage where

import Data.Sequence

data Reg = 
    A | B | C | D | E | F | G | H 
    
instance Show Reg where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show H = "h"

type CodePos = Int

type Code = Seq OpCode

data OpCode
    = HALT
    | GET
    | PUT
    | LOAD Reg
    | STORE Reg
    | RESET Reg
    | INC Reg
    | DEC Reg
    | JUMP CodePos
    | JPOS CodePos
    deriving Show
