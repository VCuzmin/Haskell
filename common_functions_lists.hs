-- The !! operator

-- If you want to access a particular element of a list by its index, you can use the !! operator. 
-- The !! operator takes a list and a number, returning the element at that location in the list. 
-- Lists in Haskell are indexed starting at 0. If you try to access a value beyond the end of the list, you’ll get an error:

GHCi> [1,2,3] !! 0
1
GHCi> "puppies" !! 4
'i'
GHCi> [1..10] !! 11
*** Exception: Prelude.!!: index too large

-- Any infix operator (an operator that’s placed between two values, such as +) can also be used like a prefix function by wrapping it in parentheses:
GHCi> (!!) [1,2,3] 0
1

-- Using prefix notation can often make things such as partial application easier. 
-- Prefix notation is also useful for using operators as arguments to other functions. 
-- You can still use partial application with an infix operator; you just need to wrap the expression in parentheses:

GHCi> paExample1 = (!!) "dog"
GHCi> paExample1 2
'g'
GHCi> paExample2 = ("dog" !!)
GHCi> paExample2 2
'g'

-- Notice that in paExample2 you see how partial application works with infix binary operators. 
-- To perform partial application on a binary operator, called a section, you need to wrap the expression in parentheses. 
-- If you include only the argument on the right, the function will be waiting for the leftmost argument; 
-- if you include only the argument on the left, you get a function waiting for the argument on the right. 
-- Here’s paExample3, which creates partial application of the right argument:

GHCi> paExample3 = (!! 2) -- a partial application of the right argument
GHCi> paExample3 "dog"
'g'

-- The important thing to remember about sections is that the parentheses aren’t optional.

-- lenght

GHCi> length [1..20]
20
GHCi> length [(10,20),(1,2),(15,16)]
3
GHCi> length "quicksand"
9

-- reverse

GHCi> reverse [1,2,3]
[3,2,1]
GHCi> reverse "cheese"
"eseehc"

isPalindrome word = word == reverse word

GHCi> isPalindrome "cheese"
False
GHCi> isPalindrome "racecar"
True
GHCi> isPalindrome [1,2,3]
False
GHCi> isPalindome [1,2,1]
True


--elem

-- The elem function takes a value and a list and checks whether the value is in the list:

GHCi> elem 13 [0,13 .. 100]
True
GHCi> elem 'p' "cheese"
False

-- elem is a function that you may want to treat as an infix operator for readability. 
-- Any binary function can be treated as an infix operator by wrapping it in back-quotes (`) or parentheses

respond phrase = if '!' `elem` phrase
                then "wow!"
                else "uh.. okay"


GHCi> respond "hello"
"uh.. okay"
GHCi> respond "hello!"
"wow!"

-- Whether infix elem adds much readability is certainly debatable, but in the real world you’ll frequently come across infix forms of binary functions.



-- take and drop
-- The take function takes a number and a list as arguments and then returns the first n elements of the list:

GHCi> take 5 [2,4..100]
[2,4,6,8,10]
GHCi> take 3 "wonderful"
"won"


-- If you ask for more values then a list has, take gives you what it can, with no error:

GHCi> take 1000000 [1]
[1]

takeLast n aList = take n (reverse aList)

GHCi> takeLast 10 [1..100]
[91,92,93,94,95,96,97,98,99,100]

-- The drop function is similar to take, except it removes the first n elements of a list:

GHCi> drop 2 [1,2,3,4,5]
[3,4,5]
GHCi> drop 5 "very awesome"
"awesome"


-- zip
-- You use zip when you want to combine two lists into tuple pairs. 
-- The arguments to zip are two lists. If one list happens to be longer, zip will stop whenever one of the two lists is empty:
GHCi> zip [1,2,3] [2,4,6]
[(1,2),(2,4),(3,6)]
GHCi> zip "dog" "rabbit"
[('d','r'),('o','a'),('g','b')]
GHCi> zip ['a' .. 'f'] [1 .. ]
[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]



-- cycle

-- The cycle function is particularly interesting, because it uses lazy evaluation to create an infinite list. 
-- Given a list, cycle repeats that list endlessly. This may seem somewhat useless but comes in handy in a surprising number of situations. 
-- For example, it’s common in numerical computing to need a list of n ones. 
-- With cycle, this function is trivial to make.


ones n = take n (cycle [1])

GHCi> ones 2
[1,1]
GHCi> ones 4
[1,1,1,1]



subSeq start end list = take difference (drop start list)
      where difference = end - start

GHCi> subseq 2 5 [1 .. 10]
[3,4,5]
GHCi> subseq 2 7 "a puppy"
"puppy"


-- returns True if an element is in the first half of a list, and otherwise returns False.
isFirstHalf x list = x `elem`(take firstHalf list)
     where firstHalf = (length list) `div` 2 


-- div and / are two different functions:

-- / is defined in class Fractional and it's meaning is an inverse operation to multiplication.
-- div is defined in class Integral and it's meaning is division of integers with truncation toward negative infinity.
-- You're right, infix notation is just a syntactic sugar. The expression x / y is the same as (/) x y, as well as div x y is the same as x `div` y.

