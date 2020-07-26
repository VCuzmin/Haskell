{-# LANGUAGE FlexibleContexts #-}

-- GREATEST COMMON DIVISOR

-- a = 20, b = 16
-- a/b = 20/16 = 1 remainder 4
-- a = 16, b = 4
-- a/b = 4 remainder 0
-- GCD = b = 4

myGCD :: Integral t => t -> t -> t
myGCD a b =
  if remainder == 0
    then b
    else myGCD b remainder
  where
    remainder = a `mod` b
myGCD a 0 = a -- version with pattern matching
myGCD a b = myGCD b (a `mod` b)

myHead :: [p] -> p
myHead (x : xs) = x
myHead [] = error "No head for empty list"

-- RECURSION ON LISTS

myLength :: Num a1 => [a2] -> a1
myLength [] = 0
myLength xs = 1 + myLength (tail xs)
--or

myLength [] = 0
myLength (x : xs) = 1 + myLength (xs)

--  Implementing take

-- The take function is interesting for two reasons: take uses two arguments, n and a list, and it turns out take has two goal states!
-- As is almost always the case, take terminates on the empty list [].
-- As mentioned earlier, unlike tail and head, take has no problem with the empty list, and will return as many items as it can.
-- The other condition when take can be finished occurs when n = 0. In either case, you end up doing the same thing.
-- Taking n elements from an empty list is [], and taking 0 elements of any list is [].
-- So you end up with this:

myTake :: Num [a1] => [a1] -> [a2] -> [a2]
myTake _ [] = []
myTake [] _ = []
-- Let’s think about this with take 3 [1,2,3,4,5]:
-- You want the first element, 1, and then cons that along with take 2 [2,3,4,5].
-- Then you want the next element, 2, and cons it with take 1 [3,4,5].
-- Then you want 3 and cons it with take 0 [4,5].
-- At 0 you’ve reached a goal, so return [].
-- This leads to 1:2:3:[], which is [1,2,3].

myTake n (x : xs) = x : rest
  where
    rest = myTake (n -1) xs

--  Implementing reverse

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : []) = [x]
myReverse (x : xs) = myReverse (xs) ++ [x]


fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

fib_v2 :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib_v2 n = fastFib 1 1 n
