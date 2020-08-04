-- foo (alpha 1) (beta 2)

-- What's hapenning here?
-- foo functions does a little bit of work and then it needs part of the result from alpha, so alpha does some more work,
-- but this doesn't necessarily finish, and then maybe foo does some more work and then beta does a little and you got the idea.
-- In fact, if foo finises and returns without using alpha of 1, alpha would never be computed at all. This is called 
-- Lazy Function Evaluation. 


intsFrom n = n : (intsFrom (n+1))
ints = intsFrom 1 -- the infinite list is not yet generated because the ints variable is not used anywhere

-- >> null ints -- False
-- >> head ints -- 1
-- >> take 10 ints -- [1,2,3,4,5,6,7,8,9,10]

removeOdd :: Integral a => [a] -> [a]
removeOdd [] = []
removeOdd (x:xs) -- version with Guards
 | (mod x 2) == 0 = x : (removeOdd xs)
 | otherwise = removeOdd xs


-- >> take 10 (removeOdd ints)
-- [2,4,6,8,10,12,14,16,18,20]

infiniteList :: [Integer]
infiniteList = intsFrom 1 

takeTen :: [Integer]
takeTen = take 10 infiniteList -- [2,4,6,8,10,12,14,16,18,20]