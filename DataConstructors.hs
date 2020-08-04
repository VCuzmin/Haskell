
-- In a data declaration, a type constructor is the thing on the left hand side of the equals sign. 
-- The data constructor(s) are the things on the right hand side of the equals sign. 
-- You use type constructors where a type is expected, and you use data constructors where a value is expected.

data Colour = Red | Green | Blue

-- Here, we have three data constructors. 
-- Colour is a type, and Green is a constructor that contains a value of type Colour. 
-- Similarly, Red and Blue are both constructors that construct values of type Colour. 
-- We could imagine spicing it up though!

data Colour' = RGB Int Int Int

--We still have just the type Colour, but RGB is not a value - it's a function takinf three Ins and returning a value!
-- RGB has the type

RGB :: Int -> Int -> Int -> Colour

-- RGB is a data constructor that is a function taking some values as its arguments, and then uses those to construct a new value.

-- In this case, if we apply RGB to three values, we get a colour value!

-- Prelude> RGB 12 92 27
-- #0c5c1b


-- We have constructed a value of type Colour by applying the data constructor. 
-- A data constructor either contains a value like a variable would, or takes other values as its argument and creates a new value. 