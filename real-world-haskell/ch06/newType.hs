data DataInt = D Int
    deriving (Eq, Ord, Show)

-- newtype: rename an existing type. used to hide the nature of a type
newtype newTypeInt = N Int
    deriving (Eq, Ord, Show)

-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

-- bad: no fields
newtype TooFew = TooFew

-- bad: more than one field
newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
newtype TooManyCtors = Bad Int
                     | Worse Int

-- Summary: the three ways of naming types
-- data    introduces a new algebraic type
-- type    gives a synonym to use of an existing type.
-- newtype gives an existing type a distinct identity. The original type and the
--         new type are not interchangeable.
