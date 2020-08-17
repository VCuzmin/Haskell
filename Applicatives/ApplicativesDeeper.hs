-- ! Look at the Applicatives2.jpg image to see the relation between Functor and Applicative

-- Applicative:
fmap :: Functor f :: (a -> b) -> f a -> f b 
(<$>) :: Functor f :: (a -> b) -> f a -> f b 
(<*>) :: Applicative f :: f (a -> b) -> f a -> f b
pure :: Applicative f :: a -> f a

-- One tricky thing in this definition is that there are two constraints on your type variable f. 
-- The first says that f is a Functor, which enforces that Applicative must be a Functor, and then you define f as your stand-in for Applicative. 

-- Notice that the operator <*> has the same type signature as your fmap, except the function argument is also in a context. 
-- This small difference in <*> allows you to chain together larger sequences of functions inside members of the Functor type class. 
-- Here are a few examples of using <$> and <*> to perform mathematical operations on Maybe types:

-- GHCi> (*) <$> Just 6 <*> Just 7
-- Just 42
-- GHCi> div <$> Just 6 <*> Just 7
-- Just 0
-- GHCi> mod <$> Just 6 <*> Just 7
-- Just 6


-- ! The pure method
-- The function pure is the second method required by the Applicative type class. 
-- The pure method is a useful helper function for taking an ordinary value or function and putting it into a context. 
-- The best way to understand pure is to play around with it in GHCi. 
-- In the example of a Maybe, pure will return a Just:

-- GHCi> pure 6 :: Maybe Int --> You life the value 6 inside of a Maybe context
-- Just 6

-- You can also use pure to put a function into the context of Applicative. 
-- For example, if you want to add 6 to (Just 5), you can use either fmap or pure:

-- GHCi> (6+) <$> Just 5
-- Just 11
-- GHCi> pure (6+) <*> Just 5
-- Just 11

-- You can rewrite this as: 
-- let f = pure (6 +) :: Maybe (Int -> Int) -- you must specify the context where you lift the function (6 +).
                                            -- you lift the (6 +) function inside of a Maybe of (Int -> Int) context
-- f <*> Just 1
-- Just 7

 -- Make the String "Hello World" into an IO String.
-- hello :: IO String
-- hello = pure "Hello World"


-- GHCI pure 6 :: [Int] - you lift 6 value inside of List of Int context
-- [6]

--  ! LIST AS A CONTEXT

-- The List type, being a fundamental example of nearly everything in Haskell, is both a container and a context. 
-- List as a container is easy to understand. 
-- List is basically a chain of buckets of whatever type of data you want to hold. 
-- But List is a member of Applicative, so there must be a way to view List as a context.

-- The reason context matters for a list is that to use Applicative, you need to be able to answer the question, “What does it mean to apply a function to two or more values in the context of a list?” 
-- For example, what does [1000,2000,3000] + [500,20000] mean? The naive assumption might be as follows:

-- [1000,2000,3000] ++ [500,20000] = [1000,2000,3000,500,20000]

-- But this would be just adding two lists, which is concatenation (the ++ operator for lists). 
-- What you’re curious about is what it means to combine two values in the context of List by using addition. 
-- In terms of Applicative, you’d read this statement as follows:

-- pure (+) <*> [1000,2000,3000] <*> [500,20000]
