{-# LANGUAGE FlexibleInstances #-}
module WithVars where
import qualified Data.Map as M
import Calc (Expr, lit, add, mul)

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

-- Using Map for adding variables
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i _ = Just i
  add expr1 expr2 m = expr1 m >>= genOp (+) (expr2 m)
  mul expr1 expr2 m = expr1 m >>= genOp (*) (expr2 m)

-- genOp generates a function that partially applies the first argument to the
-- second and returns a function that can be used as a Monad with Maybe.
genOp :: (a -> a -> a) -> Maybe a -> (a -> Maybe a)
genOp op Nothing  = \v -> Nothing
genOp op (Just v) = Just . op v

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
