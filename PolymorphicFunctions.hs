
length_ints :: Num p => [Int] -> p
length_ints [] = 0
length_ints (x:xs) = length_ints xs + 1

length_chars :: Num p => [Char] -> p
length_chars [] = 0
length_chars (x:xs) = length_chars xs + 1

--and so on

length' :: Num p => [a] -> p -- this is a polymorphic function
length' [] = 0
length' (x:xs) = 1 + length' xs

-- a is a type variable. They must start with a lower letter
-- Int, Char - concrete types. They must start with an upper letter

-- Type Class Constraints
badSum :: [a] -> a -- a could be any type, for example String but for this + operator is not defined
badSum [] = 0
badSum (x:xs) = x + badSum xs

-- >>:t sum
-- > sum :: Num a => [a] -> a

goodSum :: Num p => [p] -> p
goodSum [] = 0
goodSum (x:xs) = x + goodSum xs

-- the => (double arrow) indicates a constraint on the type variable a. It means a must be a Num type.

showSum :: (Show a, Num a) => [a] -> String
showSum xs = show (sum xs)

-- >> :t 3.1
-- >> 3.1 :: Fractional a => a