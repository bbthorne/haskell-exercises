{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler where
import Calc (Expr, lit, add, mul)
import StackVM
import Parser (parseExp)

instance Expr Program where
  lit num   = [PushI num]
  add p1 p2 = p1 ++ p2 ++ [Add]
  mul p1 p2 = p1 ++ p2 ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

test :: String -> Either String StackVal
test s = case compile s of
          Nothing -> Left "not a properly formed artithmetic expression"
          Just p  -> stackVM p
