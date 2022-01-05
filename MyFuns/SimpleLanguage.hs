module MyFuns.SimpleLanguage where

import Data.Sequence

data Reg = 
    A | B | C | D | E | F | G | H 
    
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
    | RESET Reg
    | INC Reg
    deriving Show
