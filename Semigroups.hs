 import qualified  Data.Semigroup as Semigroup


-- Syntax

--  class Semigroup a where
--      (<>) :: a -> a -> a
--      sconcat :: [[Data.List.Nonempty|Nonempty]] a -> a
--      stimes :: Integral b => b -> a -> a


    -- Methods
-- (<>) :: a -> a -> a
-- An associative binary operation.
-- sconcat :: [[Data.List.Nonempty|Nonempty]] a -> a
-- Take a nonempty list of type a and apply the <> operation to all of them to get a single result.
-- stimes :: Integral b => b -> a -> a
-- Given a number x and a value of type a, combine x numbers of the value a by repeatedly applying <>.


-- The Semigroup class has only one important method you need, the <> operator. 
-- You can think of <> as an operator for combining instances of the same type. 
-- You can trivially implement Semigroup for Integer by defining <> as +.

-- instance Semigroup Integer where
--    (<>) x y = x + y    


--  The Color Semigroup

-- Blue and yellow make green.
-- Red and yellow make orange.
-- Blue and red make purple.

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown 
             deriving (Show, Eq)

-- Next you can implement Semigroup for your Color type.

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b = if a == b
               then a
               else Brown


-- GHCi> Red <> Yellow
-- Orange
-- GHCi> Red <> Blue
-- Purple
-- GHCi> Green <> Purple
-- Brown

-- This works great, but you get an interesting problem when you add more than two colors. 
-- You want your color mixing to be associative. 
-- Associative means that the order in which you apply your <> operator doesn’t matter. 
-- For numbers, this means that 1 + (2 + 3) = (1 + 2) + 3. As you can see, your colors clearly aren’t associative:

-- GHCi> (Green <> Blue) <> Yellow
-- Brown
-- GHCi> Green <> (Blue <> Yellow)
-- Green

-- Not only does this rule about associativity make intuitive sense (mixing colors in any order should give you the same color), but this is formally required of the Semigroup type class. 
-- This can be one of the more confusing parts of the more advanced type classes we cover in this unit. 
-- Many of them have type class laws that require certain behavior. 
-- Unfortunately, the Haskell compiler can’t enforce these.


-- The binary operation <> must be associative
-- (a <> b) <> c == a <> (b <> c)
-- For example, addition (a + (b + c) == (a + b) + c), and multiplication (a * (b * c) == (a * b) * c) satisfy this requirement. 
-- Therefore <> could be defined as + or * for instances of class Num a. 
-- Division (div) however, would not be a candidate as it is not associative: 8 `div` (4 `div` 2) == 8 `div` 2 == 4 is not equal to (8 `div` 4) `div` 2 == 2 `div` 2 == 1.

-- Making Color associative and using guards
instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange 
             | otherwise = Brown

-- As you can see, now the problem is fixed:

-- GHCi> (Green <> Blue) <> Yellow
-- Green
-- GHCi> Green <> (Blue <> Yellow)
-- Green