--  SUM TYPES—COMBINING TYPES WITH “OR”

-- A die is either a 6-sided die or a 20-sided die or ....
-- A paper is authored by either a person (String) or a group of people ([String]).
-- A list is either an empty list ([]) or an item consed with another list (a:[a]).

data Bool = False | True
-- An instance of Bool is either the False data constructor or the True data constructor. 

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
   | NameWithMiddle FirstName MiddleName LastName


--    In this example, you can use two type constructors that can either be a FirstName consisting of two Strings or a NameWithMiddle consisting of three Strings. 
-- Here, using or between two types allows you to be expressive about what types mean. 