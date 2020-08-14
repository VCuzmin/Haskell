import qualified Data.Map as Map

-- qualified statement lets you give the module you're importing a name so it doesn't conflict with existing functions

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