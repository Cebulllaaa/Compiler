{-# LANGUAGE ViewPatterns #-}
module MyFuns.Values (
    SymbolTable,
    valueOf,
    getValue,
    getNumber, 
    getIdAddr,
    generateConstant,
    VarInfo(..)

) where

import Data.Map as M (Map, lookup)
import Data.Text as T (unpack)
import MyFuns.SimpleLanguage
import Gramma.Abs
import Data.List (genericLength)
import Data.List (genericReplicate)

data VarInfo
  = ScalarInfo {address :: Integer}
  | IterInfo {address :: Integer}
  | ArrayInfo {address :: Integer, begin :: Integer, end :: Integer}



maxSmallNumber :: Integer
maxSmallNumber = 3

-- | DO NOT USE REGISTER A
generateConstant :: Reg -> Integer -> [OpCode]
generateConstant A _ = error "register A is forbidden here"
generateConstant reg n
  | n >= 0 && n <= maxSmallNumber = [RESET reg] ++ genericReplicate n (INC reg)
  | n < 0 && n >= negate maxSmallNumber = [RESET reg] ++ genericReplicate n (DEC reg)
generateConstant reg n = [RESET reg, INC reg, RESET A] ++ gen n ++ [SWAP reg]
  where
    gen (-1) = [DEC A]
    gen 0 = []
    gen 1 = [INC A]
    gen k = gen q ++ [SHIFT reg] ++ gen r
      where
        (q, r) = quotRem k 2

type SymbolTable = M.Map Pidentifier VarInfo
valueOf :: Number -> Integer
valueOf (Number txt) = read $ unpack txt

getValue :: SymbolTable -> Reg -> Value -> [OpCode]
getValue _ reg (NumValue x) = getNumber reg x
getValue st reg (IdValue x) = getIdValue st reg x

getNumber :: Reg -> Number -> [OpCode]
getNumber reg x = generateConstant reg (valueOf x)

getIdValue :: SymbolTable -> Reg -> Identifier -> [OpCode]
getIdValue st reg ident = getIdAddr False st reg ident ++ [LOAD reg, SWAP reg]

getIdAddr :: Bool -> SymbolTable -> Reg -> Identifier -> [OpCode]
getIdAddr mutation st reg (LimitId pid@(Pidentifier txt)) =
  case M.lookup pid st of
    Just (IterInfo addr) ->
      if mutation then
        error $ "iterator cannot be modified: " ++ T.unpack txt
      else
        generateConstant reg (addr + 1)
    _ -> error $ "internal error: " ++ T.unpack txt
getIdAddr mutation st reg (ScalarId pid@(Pidentifier txt)) =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ScalarInfo addr) -> generateConstant reg addr
    Just (IterInfo addr) ->
      if mutation then
        error $ "iterator cannot be modified: " ++ T.unpack txt
      else
        generateConstant reg addr
    Just _ -> error $ "array used as scalar: " ++ T.unpack txt
getIdAddr _ st reg (VarArrayId pid@(Pidentifier txt) pid') =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ArrayInfo addr beg end) ->
      getIdValue st E (ScalarId pid') ++ generateConstant reg (addr - beg) ++ [SWAP reg, ADD E, SWAP reg]
    Just _ -> error $ "scalar used as array: " ++ T.unpack txt
getIdAddr _ st reg (ConstArrayId pid@(Pidentifier txt) (valueOf -> index)) =
  case M.lookup pid st of
    Nothing -> error $ "undeclared identifier: " ++ T.unpack txt
    Just (ArrayInfo addr beg end) ->
      if beg <= index && index <= end then
        generateConstant reg (addr - beg + index)
      else
        error $ "index out of range: " ++ T.unpack txt ++ "[" ++ show index ++ "]"
    Just _ -> error $ "scalar used as array: " ++ T.unpack txt

