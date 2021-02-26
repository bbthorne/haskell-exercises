module Calc where
import ExprT
import Parser
import qualified Data.Map as M

-- eval converts an expression ExprT into an Integer value
eval :: ExprT -> Integer
eval (Lit i)         = i
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

-- evalStr takes a String input and returns Just result if it is a well formed
-- arithmetic expression that evaluates to result and Nothing if it isn't.
evalStr :: String -> Maybe Integer
evalStr s = parseExp Lit Add Mul s >>= Just . eval

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 . (mod 7) $ x + y
  mul (Mod7 x) (Mod7 y) = Mod7 . (mod 7) $ x * y


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
