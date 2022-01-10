{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyFuns.SimpleLanguage where

import Data.Sequence

data Reg
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H

instance Show Reg where
  show A = "a"
  show B = "b"
  show C = "c"
  show D = "d"
  show E = "e"
  show F = "f"
  show G = "g"
  show H = "h"

newtype CodePos = CodePos Integer
  deriving Num

instance Show CodePos where
  show (CodePos x) = show x

type Code = Seq OpCode

data OpCode
  = HALT
  | GET
  | PUT
  | LOAD Reg
  | STORE Reg
  | ADD Reg
  | SUB Reg
  | SHIFT Reg
  | SWAP Reg
  | RESET Reg
  | INC Reg
  | DEC Reg
  | JUMP CodePos
  | JPOS CodePos
  | JZERO CodePos
  | JNEG CodePos
  deriving Show
