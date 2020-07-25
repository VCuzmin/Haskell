-- Plenty of binary functions have a natural order, such as division. 
-- A useful trick in Haskell is that any infix operator (such as +, /, -, *) can be used as a prefix function by putting parentheses around it:

-- GHCi> 2 + 3
-- 5
-- GHCi> (+) 2 3
-- 5
-- GHCi> 10 / 2
-- 5.0
-- GHCi> (/) 10 2
-- 5.0

-- In division and subtraction, the order of arguments is important.
-- Despite there being a natural order for the arguments, it’s easy to understand that you might want to create a closure around the second argument. 

flipBinaryArgs binaryFunc = (\x y -> binaryFunc y x)

removes2 = flipBinaryArgs (-) 2 --removes 2 from whatever number is passed in to it.

division y = \x -> (/) x y -- a closure around the second argument
