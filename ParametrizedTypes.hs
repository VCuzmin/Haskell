data Box a = Box a deriving Show
-- The Box type is an abstract container that can hold any other type. 
-- As soon as you put a type inside Box, the Box type takes on a concrete value. 
-- You can use GHCi to explore some of these:

-- GHCi> n = 6 :: Int
-- GHCi> :t Box n
-- Box n :: Box Int
-- GHCi> word = "box"
-- GHCi> :t Box word
-- Box word :: Box [Char]
-- GHCi> f x = x
-- GHCi> :t Box f
-- Box f :: Box (t -> t)
-- GHCi> otherBox = Box n
-- GHCi> :t Box otherBox
-- Box otherBox :: Box (Box Int)
wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x


data Triple a = Triple a a a deriving Show
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _ ) = x

third :: Triple a -> a
third (Triple _ _ x) = x


toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- GHCi> transform (* 3) aPoint
-- Triple 0.30000000000000004 159.60000000000002 36.900000000000006

-- GHCi> transform reverse aPerson
-- Triple "drawoH" "spillihP" "tfarcevoL"


--  Defining your own list

data List' a = Empty | Cons a (List a) deriving Show
-- “A list of type a is either Empty or the consing of the value a with another list of type a.”

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

ourMap :: (a -> b) -> List' a -> List' b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Const (func a) (ourMap func rest)


-- GHCi> ourMap (*2) ourListEx1
-- Cons 2 (Cons 4 (Cons 6 Empty))