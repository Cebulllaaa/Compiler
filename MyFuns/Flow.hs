module MyFuns.Flow (
    genCond

) where
import MyFuns.SimpleLanguage
import Gramma.Abs 
import MyFuns.Numbers (generateIncCode, valueOf)
import Debug.Trace

 
genCond :: Condition -> Int-> [OpCode]
genCond (Leq (NumValue x) (NumValue y))  pos
    | z <= 0 = [RESET A] ++ [INC A] ++ [JPOS pos]
    | otherwise = [RESET A]  -- ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)

genCond (Geq (NumValue x) (NumValue y))  pos
    | z >= 0 = [RESET A] ++ [INC A] ++ [JPOS pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)

genCond (Neq (NumValue x) (NumValue y))  pos
    | not (z == 0) = [RESET A] ++ [INC A] ++ [JPOS pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)

genCond (Le (NumValue x) (NumValue y))  pos
    | z < 0 = [RESET A] ++ [INC A] ++ [JPOS pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)

genCond (Eq (NumValue x) (NumValue y)) pos 
    | z == 0 = [RESET A] ++ [INC A] ++ [JPOS pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)
genCond (Ge (NumValue x) (NumValue y))  pos
    | z > 0 = trace("AAAA")([RESET A] ++ [INC A] ++ [JPOS pos])
    | otherwise = [RESET A] ++ [DEC A] ++ [JPOS pos]
    where z = (valueOf x) - (valueOf y)
