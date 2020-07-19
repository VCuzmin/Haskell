import Data.List

author = ("Will","Kurt") -- a tuple

-- Tuples of two items (a pair) have two useful functions, fst and snd, which access the first and second elements of the tuple, respectively:

-- GHCi> fst author
-- "Will"
-- GHCi> snd author
-- "Kurt"


names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]


-- GHCi> sort names 
-- [("Bernard","Sumner"),("Ian", "Curtis"),("Peter", "Hook"),
-- ("Stephen","Morris")]


-- Not bad, given Haskell has no idea what you’re trying to do! Unfortunately, you usually don’t want to sort by first name. 
-- To solve this, you can use Haskell’s sortBy function, which is included in the Data.List module. 
-- You need to supply sortBy with another function that will compare two of your tuple names. 
-- After you explain how to compare two elements, the rest is taken care of. 
-- For this, you write a function compareLastNames. 
-- This function takes two arguments, name1 and name2, and returns GT, LT, or EQ. GT, LT, and EQ are special values representing greater than, less than, and equal. 
-- In many programming languages, you’d return True or False, or 1, -1, or 0.

compareLastNames name1 name2 = if lastName1 > lastName2 --compare by last names
                                then GT
                                else if lastName1 < lastName2
                                    then LT
                                    else EQ
        where lastName1 = snd name1
              lastName2 = snd name2


-- GHCi> sortBy compareLastNames names
-- [("Ian", "Curtis"),("Peter", "Hook"),("Stephen","Morris),
-- ("Bernard","Sumner")]


compareLastNames name1 name2 = if lastName1 > lastName2 --compare by last names. If two last name are the same then sort by first name
                                then GT
                                else if lastName1 < lastName2
                                    then LT
                                    else if firstName1 > firstName2
                                        then GT
                                        else if firstName1 < firstName2
                                            then LT
                                            else EQ
        where lastName1 = snd name1
              lastName2 = snd name2
              firstName1 = fst name1
              firstName2 = fst name2
