-- One of the most foundational concepts in functional programming is a function without a name, called a lambda function (hence lambda calculus).
-- Lambda functions are often referred to using the lowercase Greek letter λ.
-- Another common name for a lambda function is an anonymous function.

-- GHCi> (\x -> x) 4
-- 4
-- GHCi> (\x -> x) "hi"
-- hi
-- GHCi> (\x -> x) [1,2,3]
-- [1,2,3]
-- Notice that each time you use your lambda expression, you have to redefine it.
-- This makes sense, because you have no name to call it by!
-- Lambda functions are useful but are designed to exist for only a short while.
-- In general, if a named function will do the job, it’s better to use one.

--with Where clause
sumSquareOrSquareSum :: (Ord p, Num p) => p -> p -> p
sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

--without Where clause -- hideous, horrible, ugly code! - a lot of duplication
sumSquareOrSquareSum' :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum' x y =
  if (x ^ 2 + y ^ 2) > ((x + y) ^ 2)
    then (x ^ 2 + y ^ 2)
    else (x + y) ^ 2

--better
body :: Ord p => p -> p -> p
body sumSquare squareSum =
  if sumSquare > squareSum
    then sumSquare
    else squareSum

sumSquareOrSquareSum'' :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum'' x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

--much better - with lambda function
body' :: Integer -> Integer -> Integer
body' =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum
        then sumSquare
        else squareSum
  )

sumSquareOrSquareSum''' :: Integer -> Integer -> Integer
sumSquareOrSquareSum''' x y = body' (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- Haskell has an alternative to where clauses called let expressions.
-- A let expression allows you to combine the readability of a where clause with the power of your lambda function.

sumSquareOrSquareSum'''' :: (Ord p, Num p) => p -> p -> p
sumSquareOrSquareSum'''' x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

-- Whether you choose to use let or where is a matter of style the vast majority of the time in Haskell.

-- Whenever you create a new function, named or not, you create a new scope, which is the context in which a variable is defined.
-- When a variable is used, the program looks at the nearest scope; if the definition of the variable isn’t there, it goes to the next one up.
-- This particular type of variable lookup is called lexical scope.
-- Both Haskell and JavaScript use lexical scoping, which is why IIFE and your lambda function variables behave in a similar fashion

-- Being able to use unnamed functions to create scope on the fly is an essential tool for doing much more powerful things with lambda functions