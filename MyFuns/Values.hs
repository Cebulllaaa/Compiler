{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}
module MyFuns.Values (
    SymbolTable,
    valueOf,
    getValue,
    getNumber, 
    getIdAddr,
    generateConstant,
    message,
    VarInfo(..)

) where

import Data.Map.Strict as M (Map, lookup)
import Data.Text as T (unpack)
import MyFuns.SimpleLanguage
import Gramma.Abs
import Data.List (genericLength)
import Data.List (genericReplicate)
import Data.Text (Text)

type SymbolTable = M.Map Text VarInfo

data VarInfo 
  = ScalarInfo {address :: Integer, initialized :: Bool}
  -- | Only register F can have IterInfo
  | IterInfo {address :: Integer, inRegister :: Bool}
  | ArrayInfo {address :: Integer, begin :: Integer, end :: Integer}
  deriving (Eq, Ord)

maxSmallNumber :: Integer
maxSmallNumber = 25

-- | DO NOT USE REGISTER A
generateConstant :: Reg -> Integer -> [OpCode]
generateConstant A _ = error "register A is forbidden here"
generateConstant reg n
  | n >= 0 && n <= maxSmallNumber = [RESET reg] ++ genericReplicate n (INC reg)
  | n < 0 && n >= negate maxSmallNumber = [RESET reg] ++ genericReplicate (negate n) (DEC reg)
generateConstant reg n = [RESET reg, INC reg, RESET A] ++ gen n ++ [SWAP reg]
  where
    gen (-1) = [DEC A]
    gen 0 = []
    gen 1 = [INC A]
    gen k = gen q ++ [SHIFT reg] ++ gen r
      where
        (q, r) = quotRem k 2

valueOf :: Number -> Integer
valueOf (Number txt) = read $ unpack txt

getValue :: SymbolTable -> Reg -> Value -> [OpCode]
getValue _ reg (NumValue x) = getNumber reg x
getValue st reg (IdValue x) = getIdValue st reg x

getNumber :: Reg -> Number -> [OpCode]
getNumber reg x = generateConstant reg (valueOf x)

getIdValue :: SymbolTable -> Reg -> Identifier -> [OpCode]
getIdValue st reg ident =
  case getIdAddr False st reg ident  of
    Just codeB ->
      codeB ++ [LOAD reg, SWAP reg]
    Nothing ->
      [RESET A, ADD F, SWAP reg]

message :: Pidentifier -> String -> String
message (Pidentifier ((line, col), txt)) msg =
  show line ++ ":" ++ show col ++ ": " ++ msg ++ ": " ++ T.unpack txt

lookupVar :: Pidentifier -> SymbolTable -> Maybe VarInfo
lookupVar (Pidentifier (_, txt)) st = M.lookup txt st

getIdAddr :: Bool -> SymbolTable -> Reg -> Identifier -> Maybe [OpCode]
getIdAddr mutation st reg (LimitId pid) =
  case lookupVar pid st of
    Just (IterInfo addr inRegister) ->
      if mutation then
        error $ message pid "iterator cannot be modified"
      else
          Just $ generateConstant reg (addr + 1)
    _ -> error $ message pid "getIdAddr LimitId internal error"
getIdAddr mutation st reg (ScalarId pid) =
  case lookupVar pid st of
    Nothing -> error $ message pid "undeclared identifier"
    Just (ScalarInfo addr init) ->
      if not mutation && not init then
        error $ message pid "uninitialized scalar variable"
      else 
        Just $ generateConstant reg addr
    Just (IterInfo addr inRegister) ->
      if mutation then
        error $ message pid "iterator cannot be modified"
      else
        if inRegister then 
          Nothing
        else
          Just $ generateConstant reg addr
    Just _ -> error $ message pid "array used as scalar"
getIdAddr _ st reg (VarArrayId pid pid') =
  case lookupVar pid st of
    Nothing -> error $ message pid "undeclared identifier"
    Just (ArrayInfo addr beg end) ->
      Just $ getIdValue st E (ScalarId pid') ++ generateConstant reg (addr - beg) ++ [SWAP reg, ADD E, SWAP reg]
    Just _ -> error $ message pid "scalar used as array"
getIdAddr _ st reg (ConstArrayId pid (valueOf -> index)) =
  case lookupVar pid st of
    Nothing -> error $ message pid $ "undeclared identifier"
    Just (ArrayInfo addr beg end) ->
      if beg <= index && index <= end then
        Just $ generateConstant reg (addr - beg + index)
      else
        error $ message pid "index out of range"  ++ "[" ++ show index ++ "]"
    Just _ -> error $ message pid "scalar used as array" 

