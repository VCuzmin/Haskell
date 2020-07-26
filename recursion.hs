-- GREATEST COMMON DIVISOR

-- a = 20, b = 16
-- a/b = 20/16 = 1 remainder 4
-- a = 16, b = 4
-- a/b = 4 remainder 0
-- GCD = b = 4

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b


myGCD a 0 = a   -- version with pattern matching
myGCD a b = myGCD b (a 'mod' b)


myHead (x:xs) = x
myHead [] = error "No head for empty list"