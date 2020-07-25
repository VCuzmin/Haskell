--  The most widespread of these features is that of first-class functions. 
-- These are functions that can be passed around like any other values. 
-- A decade ago, this idea was shocking to many programmers, but today the majority of programming languages support and frequently use this concept. 
-- If you’ve ever assigned an event handler in JavaScript or passed custom sort logic into a sort method in a language such as Python, you’ve already used first-class functions.

-- FUNCTIONS AS ARGUMENTS

ifEvenInc n = if even n
          then n + 1
          else n

ifEvenDouble n = if even n
                 then n*2
                 else n

ifEvenSquare n = if even n
                 then n^2
                 else n
                 

-- Although these functions are easy to write, all three are nearly identical. 
-- The only difference is in the behavior of incrementing, doubling, and squaring. What you’ve discovered here is a general pattern of computation that you can abstract away. 
-- The key thing you need to do this is the ability to pass a function as an argument to perform the desired behavior.

ifEven func n = if even n
                then func n
                else n

inc n = n+1
double n = n*2
square n = n^2

ifEvenInc_v2 n = ifEven inc n
ifEvenDouble_v2 n = ifEven double n
ifEvenSquare_v2 n = ifEven square n


-- Lambda functions as arguments
-- GHCi> ifEven (\x -> x*2) 6
-- 12



-- RETURNING FUNCTIONS

addressLetter name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

-- GHCi> addressLetter ("Bob","Smith") "PO Box 1234 - San Francisco, CA, 94111"
-- "Bob Smith - PO Box 1234 - San Francisco, CA, 94111"


sdOffice name = if lastName < "L"               -- San Francisco office
                then nameText
                   ++ " -PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109" 
      where lastName = snd name
            nameText = (fst name) ++ " " ++ lastName


nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"   -- NY office
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"   -- Reno office
  where nameText = snd name


getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sdOffice
  "reno" -> renoOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name)) -- If it’s anything else (_ is a wildcard), returns a generic solution



addressLetter_v2 name location = locationFunction name
  where locationFunction = getLocationFunction location

-- GHCi> addressLetter_v2 ("Bob","Smith") "ny"
-- "Bob Smith: PO Box 789 - New York, NY, 10013"

-- GHCi> addressLetter_v2 ("Bob","Jones") "ny"
-- "Bob Jones: PO Box 789 - New York, NY, 10013"

-- GHCi> addressLetter_v2 ("Samantha","Smith") "sf"
-- "Samantha Smith - PO Box 1010 - San Francisco, CA, 94109"

-- GHCi> addressLetter_v2 ("Bob","Smith") "reno"
-- "Smith - PO Box 456 - Reno, NV 89523"

-- GHCi> addressLetter_v2 ("Bob","Smith") "la"
-- "Bob Smith"