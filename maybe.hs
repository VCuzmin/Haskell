-- INTRODUCING MAYBE: SOLVING MISSING VALUES WITH TYPES


import qualified Data.Map as Map

-- qualified statement lets you give the module you're importing a name so it doesn't
-- conflist with existing functions

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- GHCi> Map.lookup 7 organCatalog
-- Just Heart

-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a


possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- Next you need a function to get the contents of each drawer. 
-- The following maps this list of possible drawers with the lookup function.

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
     where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\x -> x == Just organ)
                                      available)


-- GHCi> countOrgan Brain availableOrgans
-- 1
-- GHCi> countOrgan Heart availableOrgans
-- 2


isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True


justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

-- GHCi>justTheOrgans
-- [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]


isJust and isNothing
-- The Data.Maybe module contains two functions, isJust and isNothing, that solve the general case of handling Just values. 
-- isJust is identical to the isSomething function but works on all Maybe types. 
-- With Data.Maybe imported, you could’ve solved this problem as follows:

justTheOrgans' = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

-- GHCi> showOrgan (Just Heart)
-- "Heart"
-- GHCi> showOrgan Nothing
-- ""