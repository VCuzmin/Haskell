import Control.Monad
import Data.Char

-- ! BUILDING LISTS WITH THE LIST MONAD
--  Since lists are an instance of monads, you can get list comprehension in terms of the do notation.

powerOfTwo :: Int -> [Int]
powerOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

-- GHCi> powersOfTwo 10
-- [2,4,8,16,32,64,128,256,512,1024]

-- You can combine two lists easily. Suppose you want powers of 2 and 3 as n pairs.

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

--   GHCi> powersOfTwoAndThree 5
-- [(2,3),(4,9),(8,27),(16,81),(32,243)]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

-- As you can see in GHCi, you don’t get a list of size n, but rather all possible combinations of even and odd values:

-- GHCi> allEvenOdds 5
-- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]
-- GHCi> allEvenOdds 6
-- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]

valAndSquare :: [(Int, Int)]
valAndSquare = do
  val <- [1 .. 10]
  return (val, val ^ 2)

-- ! The guard function

-- Another useful trick is to filter lists.
-- Again you could use filter, but when working with monads, you’d like to be able to reason about a value outside its context.
-- In Control.Monad, a function called guard allows you to filter your values in a list.
-- You have to import Control.Monad to use guard.
-- Here’s a method of generating even numbers by using guard:

evensGuard :: Int -> [Int]
evensGuard n = do
   value <- [1 .. n]
   guard(even value)
   return value


-- ! The guard function and the Alternative type class
-- If you look at guard’s type signature, you find that it’s a strange function. 
-- Most notably, it has a type class constraint you haven’t seen before:

-- guard :: Alternative f => Bool -> f()

-- The Alternative type class is a subclass of Applicative (meaning all instances of Alternative must be instances of Applicative). 
-- But, unlike Applicative, Alternative isn’t a superclass of Monad; 
-- not all Monads are instances of Alternative. For the guard function, the key method of Alternative is empty, which works exactly like mempty from Monoid. 
-- Both List and Maybe are instances of Alternative. List’s empty value is [], and Maybe’s is Nothing. 
-- IO, however, isn’t an instance of Alternative. You can’t use guard with IO types.

-- When you first encounter guard, it might seem like magic. 
-- Surely, there must be some stateful mischief going on behind the scenes! 
-- Surprisingly, guard is a completely pure function. It’s far beyond the scope of this book, but if you feel comfortable with Monads, revisit guard and see whether you can implement it yourself. 
-- To understand guard, it helps tremendously to translate from do-notation back to >>=, >>, and lambdas. 
-- Learning about guard will also teach you a lot about the subtleties of >>. 
-- Again, this isn’t a particularly useful exercise for beginners, but highly recommended after you’re comfortable working with Monads.

-- ! LIST COMPREHENSIONS

powerOfTwo' :: Int -> [Int]
powerOfTwo' n = do
  value <- [1 .. n]
  return (2 ^ value)

-- ? List comprehensions simplify do-notation even further for generating lists.
powerOfTwo'' :: Int -> [Int]
powerOfTwo'' n = [value ^ 2 | value <- [1 .. n]]

-- The conversion is reasonably straightforward; here’s powersOfTwoAndThree converted:


powersOfTwoAndThree' ::  Int -> [(Int,Int)]
powersOfTwoAndThree' n = [(x, y) 
        | value <- [1 .. n]
        , let x = 2 ^ value
        , let y = 3 ^ value 
        ]


-- One thing that makes list comprehensions much easier to work with is that you start with the result and then show how it’s generated. 
-- It’s often easier to understand what a list comprehension is doing just by looking at the beginning of it:

allEvenOdds' :: Int -> [(Int,Int)]
allEvenOdds' n = [(evenValue,oddValue) |  evenValue <- [2,4 .. n]
                                      ,  oddValue <- [1,3 .. n]]


-- The guard function is completely abstracted out of list comprehensions:

evensGuard' :: Int -> [Int]
evensGuard' n = [ value | value <- [1 .. n], even value]


-- ? Write a list comprehension that takes the following words

-- ? ["brown","blue","pink","orange"]
-- ? and capitalizes the first letter, and prepends Mr. in front. (Hint: use Data.Char’s toUpper.)

answer :: [String]
answer = ["Mr. " ++ capVal | val <-
                                 ["brown","blue","pink","organge","white"]
                                 , let capVal = (\(x:xs) ->
                                                  toUpper x:xs) val]