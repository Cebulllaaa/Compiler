module MyFuns.Flow (
    genCond,
    genUntilCode

) where
import MyFuns.SimpleLanguage
import Gramma.Abs 
import MyFuns.Numbers (generateIncCode, valueOf)
import Debug.Trace


genUntilCode :: Condition -> Int -> [OpCode]
genUntilCode (Eq u v) pos = genCond (Neq u v) (-1*pos)
genUntilCode (Neq u v) pos = genCond (Eq u v) (-1*pos)
genUntilCode (Leq u v) pos = genCond (Ge u v) (-1*pos)
genUntilCode (Geq u v) pos = genCond (Le u v) (-1*pos)
genUntilCode (Le u v) pos = genCond (Geq u v) (-1*pos)
genUntilCode (Ge u v) pos = genCond (Leq u v) (-1*pos)
    
 
genCond :: Condition -> Int-> [OpCode]
genCond (Leq (NumValue x) (NumValue y))  pos
    | z <= 0 = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A]  ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)

genCond (Geq (NumValue x) (NumValue y))  pos
    | z >= 0 = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)

genCond (Neq (NumValue x) (NumValue y))  pos
    | not (z == 0) = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)

genCond (Le (NumValue x) (NumValue y))  pos
    | z < 0 = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)


genCond (Eq (NumValue x) (NumValue y)) pos 
    | z == 0 = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)
    
genCond (Ge (NumValue x) (NumValue y))  pos
    | z > 0 = [RESET A] ++ [INC A] ++ [JNEG pos]
    | otherwise = [RESET A] ++ [DEC A] ++ [JNEG pos]
    where z = (valueOf x) - (valueOf y)
