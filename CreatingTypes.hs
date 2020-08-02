-- USING TYPE SYNONYMS

-- We mentioned that in Haskell you can replace the [Char] type with String. 
-- From Haskell’s perspective, these are two names for the same thing.
-- When you have two names for the same type, it’s referred to as a TYPE SYNONIM. 
-- Type synonyms are extremely useful, because they make reading type signatures much easier. 

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
            where ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
                  name = lname ++ ", " ++ fname


-- If you assume that patientInfo is part of a larger application, it’s likely that first name, last name, age, and height will be used frequently. 
-- Type signatures in Haskell are of much more benefit to the programmer than the compiler. 

-- In Haskell, you can create new type synonyms by using the type keyword. Here’s the code to create the type synonyms you’d like.

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- You can rewrite the original type signature now as follows:

patientInfo_v2 :: FirstName -> LastName -> Age -> Height -> String
patientInfo_v2 fname lname age height = name ++ " " ++ ageHeight
            where ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
                  name = lname ++ ", " ++ fname


type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo_v3 :: PatientName -> Int -> Int -> String
patientInfo_v3 (fname,lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"




--  CREATING NEW TYPES
-- Creating a new type can be done with the `data` keyword

data Sex = Male | Female --Male or Female like Maybe

-- Sex - Type constructor
-- Male, Female - Data constructors

-- In this new type, you define a few key pieces. The data keyword tells Haskell that you’re defining a new type. 
-- The word Sex is the type constructor. In this case, the type constructor is the name of the type, but in later lessons you’ll see that type constructors can take arguments. 
-- Male and Female are both data constructors. A data constructor is used to create a concrete instance of the type. 
-- By separating the data constructors with |, you’re saying, “The Sex type can be either Male or an instance of Female.”

data Bool = True | False

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'


data RhType = Pos | Neg --Rh type blood

data ABOType = A | B | AB | O -- ABO blood group

data BloodType = BloodType ABOType RhType
--BloodType is made by combining an ABOType and an RhType

-- Notice that in this case, the data constructor has the same name as your type constructor. 
-- If your type has only one constructor, it is common to re-use the name of the type as the constructor name.
-- It doesn’t have to, but in this case it makes sense. You need this data constructor to combine your ABOType and RhType. 
-- You can read the data constructor as “A BloodType is an ABOType with an RhType.”

-- Now you’re able to create BloodType data:

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- Notice that you’re able to use pattern matching in the last step to easily extract the ABOType and RhType components of BloodType.

-- The rules for blood type matching are as follows:

-- A can donate to A and AB.
-- B can donate to B and AB.
-- AB can donate only to AB.
-- O can donate to anybody.


canDonateTo :: BloodType -> BloodType -> Prelude.Bool
canDonateTo (BloodType O _) _ = Prelude.True -- universal donor
canDonateTo _ (BloodType AB _) = Prelude.True -- universal receiver
canDonateTo (BloodType A _) (BloodType A _) = Prelude.True
canDonateTo (BloodType B _) (BloodType B _) = Prelude.True
canDonateTo _ _ = Prelude.False -- otherwise


type MiddleName = String
data Name = Name FirstName LastName 
            | NameWithMiddle FirstName MiddleName LastName

-- You can read this definition of Name as follows: a Name is either a first and last name, or a name with a middle name included. 
-- You can use pattern matching to create a showName function that works with either constructor.


showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"


-- SUM TYPES
-- We refer to a type with multiple data constructors as a “sum” type.

data Employee =
 Executive String Int Int |
 VicePresident String String Int |
 Manager String String |
 Engineer String Int

-- If your type has only one data constructor, it is common to re-use the name of the type as the constructor name:

data Employee_OneDataConstructor = Employee String Int



-- USING RECORD SYNTAX
-- The first step in modeling a patient should be to list all the features you want to keep track of along with the type that should represent them:

-- Name: Name
-- Sex: Sex
-- Age (years): Int
-- Height (inches): Int
-- Weight (pounds): Int
-- Blood type: BloodType
-- You can now use the data keyword to create a new type that represents this information just as you did for blood type.

data Patient = Patient Name Sex Int Int Int BloodType
johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient  _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

-- And so on for the others properties. But writing a getter for each propery could become annoying!
-- Haskell has a great solution to this problem.
-- You can define data types such as Patient by using record syntax. 
-- Defining a new data type by using record syntax makes it much easier to understand which types represent which properties of the data type.

data Patient_WithRecordSintax = Patient_WithRecordSintax {name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType}


jackieSmith = Patient_WithRecordSintax {name = Name "Jackie" "Smith"
                                        , age = 43
                                        , sex = Female
                                        , height = 62
                                        , weight = 115
                                        , bloodType = BloodType O Neg}

-- In addition, you don’t have to write your getters; 
-- each field in the record syntax automatically creates a function to access that value from the record:
-- GHCi> height jackieSmith
-- 62
-- GHCi> showBloodType (bloodType jackieSmith)
-- "O-"


-- You can also set values in record syntax by passing the new value in curly brackets to your data. 
-- Suppose you have to update Jackie Smith’s age because of her birthday. 
-- Here’s how you could do this using record syntax.

jackieSmithUpdated = jackieSmith {age = 44} -- a new Patient type will be created
-- Because you’re still in a purely functional world, a new Patient type will be created and must be assigned to a variable to be useful.

donorFor :: Patient_WithRecordSintax -> Patient_WithRecordSintax -> Prelude.Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)