import qualified Data.Map as Map   

-- COMPUTING IN A MAYBE

-- fmap :: Functor f => (a -> b) -> (f a -> f b)
-- Notice that we’re using <$>, which is a synonym for fmap (except it’s a binary operator rather than a function).

data Maybe' a = Just' a | Nothing'

instance Show a => Show (Maybe' a) where
  show (Just' a) = show a
  show Nothing' = ""

incWithOneMaybe :: Maybe' Int -> Maybe' Int
incWithOneMaybe (Just' n) = Just' (n + 1)
incWithOneMaybe Nothing' = Nothing'

incWithTwoMaybe :: Maybe' Int -> Maybe' Int
incWithTwoMaybe (Just' n) = Just' (n + 2)
incWithTwoMaybe Nothing' = Nothing'

incMaybe :: (a -> b) -> Maybe' a -> Maybe' b
incMaybe func (Just' n) = Just' (func n)
incMaybe func Nothing' = Nothing'

--and so on

-- You can define fmap as a generalization of your custom incMaybe function.
instance Functor Maybe' where
    fmap func (Just' n) = Just' (func n)
    fmap func Nothing' = Nothing'

-- With fmap, you no longer need a special function for keeping your value in a Maybe:

-- GHCi> fmap (+ 1) (Just 6)
-- Just 7
-- GHCi> fmap (+ 1) Nothing
-- Nothing

-- Though fmap is the official function name, in practice the binary operator <$> is used much more frequently:

-- GHCi> (+ 1) <$> (Just 6)
-- Just 7
-- GHCi> (+ 1) <$> Nothing 
-- Nothing




-- In this example, (+ 1) adds 1 into the Maybe Int and returns a Maybe Int as well. 
-- But it’s important to realize that the type signature of the function in fmap is (a -> b), meaning that the Maybe returned doesn’t need to be parameterized by the same type.


data RobotPart = RobotPart 
  {
      name :: String
      , description :: String
      , cost :: Double
      , count :: Int
  } deriving Show

  leftArm :: RobotPart
  leftArm = RobotPart {
      name = "left arm"
      , description = "left arm for face punching"
      , cost = 1000.00
      , count = 3
  }


rightArm :: RobotPart
rightArm  = RobotPart
   { name = "right arm"
   , description = "right arm for kind hand gestures"
   , cost = 1025.00
   , count = 5
   }

robotHead :: RobotPart
robotHead  = RobotPart
   { name = "robot head"
   , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
   }

-- One of the most common things you’ll need to do is to render the information contained in a RobotPart as HTML. 
-- Here’s code for rendering an individual RobotPart as an HTML snippet.

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",partName, "</h2>"
                          ,"<p><h3>desc</h3>",partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount,"</p>"]
 where partName = name part
       partDesc = description part
       partCost = show (cost part)
       partCount = show (count part)


-- In many cases, you’ll want to convert a RobotPart into an HTML snippet. 
-- Next you’ll see four scenarios of this, using different parametrized types.

-- You’ll start by using the Map type to create partsDB, which is your internal database of RobotParts.

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm,robotHead]
        keyVals = zip keys vals
