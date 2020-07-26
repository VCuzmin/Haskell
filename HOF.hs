-- A higher-order function is technically any function that takes another function as an argument.add3ToAll [] = []

add3ToAll :: Num a => [a] -> [a]
add3ToAll (x : xs) = (3 + x) : add3ToAll xs

mul3ByAll :: Num a => [a] -> [a]
mul3ByAll [] = []
mul3ByAll (x : xs) = (3 * x) : mul3ByAll xs

-- USING MAP

-- GHCi> map reverse ["dog","cat", "moose"]
-- ["god","tac","esoom"]
-- GHCi> map head ["dog","cat", "moose"]
-- "dcm"
-- GHCi> map (take 4) ["pumpkin","pie","peanut butter"]
-- ["pump","pie","pean"]

-- A common first impression most programmers have of map is that it’s a cleaner version of a for loop.

myMap :: (t -> a) -> [t] -> [a]
myMap f [] = []
myMap f (x : xs) = (f x) : myMap f xs

--  FILTERING A LIST
-- GHCi> filter even [1,2,3,4]
-- [2,4]
-- GHCi> filter (\(x:xs) -> x == 'a') ["apple","banana","avocado"]
-- ["apple","avocado"]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

remove :: (a -> Bool) -> [a] -> [a] -- removes elements that pass the test
remove test [] = []
remove test (x : xs) =
  if test x
    then remove test xs
    else x : remove test xs




-- FOLDING A LIST

-- The function foldl (the l stands for left, which we’ll explain soon) takes a list and reduces it to a single value. 
-- The function takes three arguments: a binary function, an initial value, and a list. 
-- The most common use of foldl is to sum a list:

-- GHCi> foldl (+) 0 [1,2,3,4]
-- 10

myProduct :: (Foldable t, Num b) => t b -> b
myProduct xs = foldl (*) 1 xs


concatAll :: Foldable t => t [Char] -> [Char]
concatAll xs = foldl (++) "" xs

sumOfSquares :: Num b => [b] -> b
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons :: [a] -> a -> [a]
rcons x y = y:x

myReverse :: Foldable t => t a -> [a]
myReverse xs = foldl rcons [] xs

myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x


myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs


-- GHCi> foldl (+) 0 [1,2,3,4]
-- 10
-- GHCi> foldr (+) 0 [1,2,3,4]
-- 10


-- But for subtraction, order does matter:

-- GHCi> foldl (-) 0 [1,2,3,4]
-- -10
-- GHCi> foldr (-) 0 [1,2,3,4]
-- -2