-- All types in Haskell start with a capital letter to distinguish them from functions (which all start with a lowercase letter or _). 

x :: Int
x = 2

-- GHCi> x*2000
-- 4000
-- GHCi> x^2000
-- 0

-- As you can see, Haskell handles exceeding the bounds of the Int by returning 0. 
-- This property of having limited maximum and minimum values is referred to as being bounded.

y :: Integer
y = 2

-- GHCi> y*2000
-- 4000
-- GHCi> y^2000
-- 11481306952742545242328332011776819840223177020886952004776427368257662613
-- 923703138566594863165062699184459646389874627734471189608630553314259313561
-- 666531853912998914531228000068877914824004487142892699006348624478161546364
-- 638836394731702604046635397090499655816239880894462960562331164953616422197
-- 033268134416890898445850560237948480791405890093477650042900271670662583052
-- 200813223628129176126788331720659899539641812702177985840404215985318325154
-- 088943390209192055495778358967203916008195721663058275538042558372601552834
-- 878641943205450891527578388262517543552880082284277081796545376218485114902
-- 9376

-- As you can see, the Integer type fits more closely with the mathematical sense of what an integer is: any whole number. 
-- Unlike the Int type, the Integer type isn’t bounded by memory limitations framed in terms of bytes.

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.7,0.8]

letters :: [Char]
letters = ['a','b','c']

-- A list of characters is the same as a string:

-- GHCi> letters == "abc"
-- True

-- To make things easier, Haskell allows you to use String as a type synonym for [Char]. Both of these type signatures mean exactly the same thing to Haskell:

-- aPet :: [Char]
-- aPet = "cat"

-- anotherPet :: String
-- anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34,74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch",'D')

streetAddress :: (Int, String)
streetAddress = (123,"Happy St.")


--  FUNCTION TYPES
-- Functions also have type signatures. In Haskell an -> is used to separate arguments and return values.

half :: Int -> Double
half n = n/2 --  Incorrect code!


-- r. The problem is that you’re trying to divide a whole number Int in half, and such a thing is nonsensical because you’ve already declared that you’re going to return a Double. 
-- You need to convert your value from an Int into a Double. Most programming languages have the idea of casting a variable from one type to another. 
-- Casting forces a value to be represented as a different type. 
-- Because of this, casting variables often feels like hammering a square peg through a round hole. 
-- Haskell has no convention for casting types and instead relies on functions that properly transform values from one type to another. 
-- In this case, you can use Haskell’s fromIntegral function:

half' n = (fromIntegral n) /2

-- Here you’ve transformed n from an Int into a more general number. 
-- A good question now might be, “Why don’t you have to call fromIntegral on 2?” 
-- In many programming languages, if you want to treat a literal number as a Double, you need to add a decimal to it. 
-- In both Python and Ruby, 5/2 is 2 and 5/2.0 is 2.5. 
-- Haskell is both stricter and more flexible. 
-- It’s stricter because Haskell never does the implicit type conversion that happens in Ruby and Python, and it’s more flexible because in Haskell literal numbers are polymorphic: their type is determined from the compiler based on the way they’re used. 
-- For example, if you want to use GHCi as a calculator, you’ll find you rarely need to worry about type with numbers:

-- GHCi> 5/2
-- 2.5


halve :: Integer -> Integer
halve value = value `div` 2
-- Haskell has a function named div that does perform integer division (it returns only whole numbers).
-- 5 `div` 2
-- 2

--  Functions for converting to and from strings

GHCi> show 6
"6"
GHCi> show 'c'
"'c'"
GHCi>show 6.0
"6.0"

-- The read function works by taking a string and converting it to another type. But this is a bit trickier than show. 
-- For example, without type signatures, what should Haskell do here?

-- z = read "6"

-- It’s impossible to tell whether to use Int, Integer, or even Double. 
--If you can’t figure it out, there’s absolutely no way that Haskell can. 
-- In this case, type inference can’t save you. There are a few ways to fix this. 
-- If you use the value z, it’s likely that Haskell will have enough info to figure out how to treat your value:

q = z / 2

-- Now Haskell has enough information to treat z like a Double, even though your String representation didn’t have a decimal. Another solution is to explicitly use your type signature.

anotherNumber :: Int
anotherNumber = read "6"

GHCi> read "6" :: Int
6
GHCi> read "6" :: Double
6.0

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n


-- TYPE VARIABLES
-- We’ve covered a bunch of common types and how they work in functions. 
-- But what about the simple function, which returns any value that’s passed in to it? 
-- Really, simple could take any type of argument at all. 
-- Given what you know so far, you’d have to make a family of simple functions to work with every type.

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

-- But this is ridiculous, and clearly not how Haskell works, because type inference was able to understand simple. 
-- To solve this problem, Haskell has type variables. Any lowercase letter in a type signature indicates that any type can be used in that place. 
-- The type definition for simple looks like the following.

simple :: a -> a
simple x = x

-- Type variables are literally variables for types. 
-- Type variables work exactly like regular variables, but instead of representing a value, they represent a type. 
-- When you use a function that has a type variable in its signature, you can imagine Haskell substituting the variable that’s needed

-- Type signatures can contain more than one type of variable. 
-- Even though the types can be any value, all types of the same variable name must be the same. 
-- Here’s an example of a function that makes triples (tuples with three values).

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)

-- The reason for different names for type variables is the same as using different names for regular variables: they may contain different values. 
-- In the case of makeTriple, you can imagine a case in which you have a String, a Char, and another String:

nameTriple = makeTriple "Oscar" 'D' "Grouch"
-- In this example, you can imagine that the type signature that Haskell uses looks like this:

makeTriple :: String -> Char -> String -> (String, Char, String)

f1 :: a -> a
f2 :: a -> b
-- You know that f2 is a function that can produce a much wider range of possible values. 
-- The f1 function could behave only by changing a value and keeping it as the same type: Int -> Int, Char -> Char, and so forth. 
-- In contrast, f2 can represent a much broader range of possible behaviors: Int -> Char, Int -> Int, Int -> Bool, Char -> Int, Char -> Bool, and so forth.


-- The type signature for map is as follows:

map :: (a -> b) -> [a] -> [b]

-- Why couldn’t it be this?

map :: (a -> a) -> [a] -> [a]?

map:: (a -> a) -> [a] -> [a] would mean that map must always return the same type as it currently is.

-- In this case, you couldn’t perform

map show [1,2,3,4]

-- because show returns a type String that isn’t consistent with the original type. 
-- The real power of map isn’t iteration, but transforming a list of one type into a list of another type.

-- Help the Compiler
x = show (read "123") -- Error
x' = show (read "123" :: Int) -- correct because this time the read method knows to convert "123" to an Int
