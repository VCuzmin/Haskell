--  PRODUCT TYPES—COMBINING TYPES WITH “AND”

-- A fraction can be defined as a numerator (Integer) and denominator (another Integer).
-- A street address might be a number (Int) and a street name (String).
-- A mailing address might be a street address and a city (String) and a state (String) and a zip code (Int).


-- Although the name product type might make this method of combining types sound sophisticated, this is the most common way in all programming languages to define types. 
-- Nearly all programming languages support product types. 
-- The simplest example is a struct from C. Here’s an example in C of a struct for a book and an author.


data AuthorName = AuthorName String String

-- Or

data AuthorName' = AuthorName' { -- rewrite AuthorName by using record sintax
     firstName :: String
   , lastName :: String
}

data Book = Book {
     author  :: AuthorName
   , isbn    :: String
   , title   :: String
   , year    :: Int
   , price   :: Double}


-- Book and AuthorName are examples of product types and have an analog in nearly every modern programming language. 
-- What’s fascinating is that in most programming languages, combining types with an and is the only way to make new types.



type Radius = Double
type Height = Double
type Width = Double

data Shape = Circle Radius | Square Height | Rectangle Height Width deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square h) = 4 * h
perimeter (Rectangle h w) = 2 * h + 2 * w


area :: Shape -> Double
area (Circle r) = pi*r^2
area (Square h)  = h^2
area (Rectangle h w) = h*w