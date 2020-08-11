elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' x (y : ys)
   | x == y  = True
   | otherwise = elem' x ys

-- Eq t is a contraint that say that t must be an instance of type Eq(it means it must have the == operator defined)

data RGB = RGB Int Int Int
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors :: Bool
greenInColors = elem' green colors --this won't worj because the RGB type is not in the Eq type class, is not an instance of Eq type class
-- The solution is to create an instance of Eq type class for RGB type

instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) = 
        (r1 == r2) && (g1 == g2) && (b1 == b2)
-- The closest analogy in the object-oriented world is to think about Eq as an interface and this type class instance

-- >> elem' green colors
-- >> True

instance Show RGB where
    show (RGB r g b) = 
        "RGB " ++ (show r) ++ " " ++ 
        (show g) ++ " " ++ (show b)


data Maybe' a = Nothing' | Just' a

instance (Eq a) => Eq Maybe' a where 
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) == (Just' y) = x == y
-- (Eq a) - type constraint to the Eq instance. Type a must be an instance of Eq type class. This is called THE CONTEXT OF A TYPE CLASS INSTANCE.


 -- Deriving type class instances

data RGB' = RGB' Int Int Int

instance Eq RGB' where
    (RGB' r1 g1 b1) == (RGB' r2 g2 b2) = 
        (r1 == r2) && (g1 == g2) && (b1 == b2)

-- or

data RGB'' = RGB'' Int Int Int deriving Eq -- test each of the components of a type are equal - like the one above

-- both of them are the same


-- Defining type classes

data Point2 = Point2 Double Double
data Point3 = Point3 Double Double Double deriving Show -- Point3 is an instance of Show type class.(Point3 derive from Show)
data Point4 = Point4 Double Double Double Double deriving Show

distance2 :: Point2 -> Point2 -> Double
distance2 (Point2 x1 y1) (Point2 x2 y2) = 
    sqrt(dx * dx + dy * dy)
    where dx = x1 - x2
          dy = y1 - y2


distance3 :: Point3 -> Point3 -> Point3 -> Double
distance3 (Point3 x1 y1 z1) (Point3 x2 y2 z2) = 
    sqrt(dx * dx + dy * dy + dz * dz)
    where dx = x1 - x2
          dy = y1 - y2
          dz = z1 - z2

-- If I want to compute the distance for two points in 4-axis dimension, I would have to define the distance4 function

class Measurable  a where -- it seems like an abstract class from OOP 
    distance :: a -> a -> Double

instance Measurable Point2 where
    distance = distance2

instance Measurable Point3 where
    distance = distance3

instance Measurable Point4 where
    distance (Point4 x1 y1 z1 q1) (Point4 x2 y2 z2 q2) = 
        sqrt (dx * dx + dy * dy + dz * dz + dq * dq)
        where dx = x1 - x2
              dy = y1 - y2
              dz = z1 - z2
              dq = q1 - q2


instance Measurable Double where
    distance x y = abs (x - y) -- the distance between two numbers




-- Subclasses of type classes

class (Measurable a, Show a) => Directions a where 
    getDirections :: a -> a -> String
    getDirections p1 p2 =  -- this is the default implementation of getDirections function
        "Go from " ++ (show p1) ++ 
        " towards " ++ (show p2) ++
        " and stop after " ++ (show (distance p1 p2))

instance Directions Point3 where -- here the Point3 type must be an instance of Measurable and Shop types
    getDirections p1 p2 = 
        "Fly from " ++ (show p1) ++
        " towards " ++ (show p2) ++
        " and stop after " ++ (show (distance p1 p2))


instance Directions Point2 where -- for Point2 type we use the default implemenation of getDirections. This measns we don't actually
-- have to define any functions to make Point2 an instance of Directions but we still need an empty declaration like this.





data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- GHCi> Chocolate
-- Chocolate
-- GHCi> Vanilla
-- Vanilla

-- GHCi> Vanilla == Vanilla
-- True
-- GHCi> Chocolate == Vanilla
-- False
-- GHCi> Chocolate /= Vanilla
-- True


-- IMPLEMENTING SHOW

data SixSidedDice = S1 | S2 | S3 | S4 | S5 | S6 deriving Show
-- GHCi> S1   --these are the default implementation
-- S1
-- GHCi> S2
-- S2
-- GHCi> S3
-- S3
-- GHCi> S4
-- S4

instance Show SixSidedDice where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- GHCi> S1
-- one
-- GHCi> S2
-- two
-- GHCi> S6
-- six


--  DEFAULT IMPLEMENTATION AND MINIMUM COMPLETE DEFINITIONS
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool


-- You have to implement only two methods: the Equals method (==) and the Not Equals method (/=). 
-- Given how smart Haskell has been so far, this should seem like more work than makes sense. 
-- After all, if you know the definition of (==), the definition of (/=) is not (==). 
-- Sure, there may be some exceptions to this, but it seems that in the vast majority of cases, if you know either one, then you can determine the other.

-- It turns out that Haskell is smart enough to figure this out. 
-- Type classes can have default implementations of methods. 
-- If you define (==), Haskell can figure out what (/=) means without any help.

instance Eq SixSidedDice where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False


-- In GHCi, you’ll see that (/=) works automatically!

-- GHCi> S6 == S6
-- True
-- GHCi> S6 == S5
-- False
-- GHCi> S5 == S6
-- False
-- GHCi> S5 /= S6
-- True
-- GHCi> S6 /= S6
-- False



--  IMPLEMENTING ORD

-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a


-- The compare method takes two values of your type and returns Ordering. 

-- data Ordering = LT | EQ | GT

instance Ord SixSidedDice where
    compare S6 S6 = EQ
    compare S6 _ = GT
    compare _ S6 = LT
    compare S5 S5 = EQ
    compare S5 _ = GT
    compare _ S5 = LT
    compare S4 S4 = EQ
    compare _ S4 = LT