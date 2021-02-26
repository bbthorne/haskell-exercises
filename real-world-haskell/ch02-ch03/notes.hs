-------------------------------------ch02---------------------------------------
-- simple function that translates an infix op
add a b = a + b

-- drop takes a natural number n and a list and returns a list without the first
-- n elements.
myDrop n []     = []
myDrop 0 xs     = xs
myDrop n (x:xs) = myDrop (n - 1) xs

-- last_but_one takes a list and returns the element before the last
last_but_one (x:(y:[])) = x
last_but_one (x:xs)     = last_but_one xs

-------------------------------------ch03---------------------------------------
-- Bookstore example

-- for custom datatypes: Type = ValueConstructor field0 ... fieldn
-- ValueConstructor is a function of type field0 -> ... -> fieldn -> Type
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

book = Book 420 "A book title" ["author0", "author1"]

-- synonyms: give a type a different name
type CustomerID = Int
type ReviewBody = String

-- It is common to give the Type and ValueConstructor the same name
data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address    = [String]

-- Algebraic Data Types have many value constructors separated by "|"
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- 2 Different datatypes for 2D vectors
-- deriving from Eq allows us to compare values of this type with "=="
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

-- Unions in Haskell are much safer:
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]

-- Pattern Matching
sumlist (x:xs) = x + sumlist xs
sumlist []     = 0

-- Applying functions to values that cannot match results in a runtime error
-- ex: this functions cannot match "False" as the first elt of the tuple
complicatedEx (True, a, x:xs, 5) = (a, xs)

-- Accessor functions for the Book type using wildcards.
-- "Boilerplate" approach: bulky, not ideal
bookID      (Book id _ _) = id
bookTitle   (Book _ title _) = title
bookAuthors (Book _ _ authors) = authors

-- Can define a type with accessor functions:
-- customerID      :: Customer -> CustomerID
-- customerName    :: Customer -> String
-- customerAddress :: Customer -> Address
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

-- 2 ways to define a Customer:
customer1 = Customer 1 "Ben" ["Fake st", "Fake city", "Fake country"]

-- using record syntax, we can vary the order of the fields
customer2 = Customer {
              customerID      = 2
            , customerAddress = ["Fake rd", "New Fake City", "Fake country"]
            , customerName    = "Grace"
            }

-- Parameterized Types: a is a type variable, we can use Perhaps on any type
data Perhaps a = Only a
               | NotIt
                 deriving (Show)

someBool   = Only True
someString = Only "something"

-- Recursive Types:
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- List a is isomorphic to [a]
from_list (x:xs) = Cons x (from_list xs)
from_list []     = Nil

from_List (Cons x xs) = x:from_List xs
from_List Nil         = []

-- Binary Tree type
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Empty
                    deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

data AltBinTree a = AltNode (Perhaps (a, AltBinTree a, AltBinTree a))

altTree = AltNode (Only ("parent",
                         (AltNode (Only ("left child", AltNode NotIt, AltNode NotIt))),
                         (AltNode (Only ("right child", AltNode NotIt, AltNode NotIt)))))

-- Throwing Exceptions: risky and not descriptive
my_second (_:y:_) = y
my_second _       = error "list too short"

-- Use Maybe type instead, let caller decide what to do with Nothing
safe_second :: [a] -> Maybe a
safe_second (_:y:_) = Just y
safe_second _       = Nothing

-- let
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- where - define local variables after an expression
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount

-- defining local functions
pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "1 " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- don't need to follow the offside rule with explicit structuring:
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1; b = 2; c = 3}
      in a + b + c

-- foo and bar are identical, but bar is better and more common

-- case
fromMaybe defval wrapped =
    case wrapped of
      Nothing    -> defval
      Just value -> value

-- guards are expressions of type Bool, enforces a condition on a pattern match
nodes_are_same (Node a _ _) (Node b _ _)
    | a == b       = Just a
nodes_are_same _ _ = Nothing

-- "otherwise" bound to True
lend3 amount balance
    | amount <= 0            = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise              = Just newBalance
  where reserve    = 100
        newBalance = balance - amount

-- Note: camelcase for everything is the convention
