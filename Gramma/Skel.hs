-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Gramma.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Gramma.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transPidentifier :: Gramma.Abs.Pidentifier -> Result
transPidentifier x = case x of
  Gramma.Abs.Pidentifier string -> failure x

transNumber :: Gramma.Abs.Number -> Result
transNumber x = case x of
  Gramma.Abs.Number string -> failure x

transProgram :: Gramma.Abs.Program -> Result
transProgram x = case x of
  Gramma.Abs.Program declarations commands -> failure x

transDeclaration :: Gramma.Abs.Declaration -> Result
transDeclaration x = case x of
  Gramma.Abs.ScalarDecl pidentifier -> failure x
  Gramma.Abs.ArrayDecl pidentifier number1 number2 -> failure x

transCommand :: Gramma.Abs.Command -> Result
transCommand x = case x of
  Gramma.Abs.Assign identifier expression -> failure x
  Gramma.Abs.IfElse condition commands1 commands2 -> failure x
  Gramma.Abs.IfElseSkip condition commands -> failure x
  Gramma.Abs.While condition commands -> failure x
  Gramma.Abs.Repeat commands condition -> failure x
  Gramma.Abs.ForTo pidentifier value1 value2 commands -> failure x
  Gramma.Abs.ForDownTo pidentifier value1 value2 commands -> failure x
  Gramma.Abs.Read identifier -> failure x
  Gramma.Abs.Write value -> failure x

transExpression :: Gramma.Abs.Expression -> Result
transExpression x = case x of
  Gramma.Abs.ValueExpr value -> failure x
  Gramma.Abs.Plus value1 value2 -> failure x
  Gramma.Abs.Minus value1 value2 -> failure x
  Gramma.Abs.Times value1 value2 -> failure x
  Gramma.Abs.Div value1 value2 -> failure x
  Gramma.Abs.Mod value1 value2 -> failure x

transCondition :: Gramma.Abs.Condition -> Result
transCondition x = case x of
  Gramma.Abs.Eq value1 value2 -> failure x
  Gramma.Abs.Neq value1 value2 -> failure x
  Gramma.Abs.Le value1 value2 -> failure x
  Gramma.Abs.Ge value1 value2 -> failure x
  Gramma.Abs.Leq value1 value2 -> failure x
  Gramma.Abs.Geq value1 value2 -> failure x

transValue :: Gramma.Abs.Value -> Result
transValue x = case x of
  Gramma.Abs.NumValue number -> failure x
  Gramma.Abs.IdValue identifier -> failure x

transIdentifier :: Gramma.Abs.Identifier -> Result
transIdentifier x = case x of
  Gramma.Abs.ScalarId pidentifier -> failure x
  Gramma.Abs.VarArrayId pidentifier1 pidentifier2 -> failure x
  Gramma.Abs.ConstArrayId pidentifier number -> failure x
