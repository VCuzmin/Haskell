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
leftArm  = RobotPart
   { name = "left arm"
   , description = "left arm for face punching!"
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


-- Converting a Maybe RobotPart to Maybe Html

-- Now suppose you have a website driven by partsDB. 
-- It’s reasonable that you’d have a request containing an ID for a part that you wish to insert into a web page. 
-- You’ll assume that an insertSnippet IO action will take HTML and insert it into a page’s template. 
-- It’s also reasonable to assume that many data models might be generating snippets. 
-- Because any one of these models may have an error, you’ll assume that insertSnippet accepts Maybe Html as its input, allowing the template engine to handle missing snippets as it sees fit. 
-- Here’s the type signature of your imaginary function:

-- inserSnippet :: Maybe Html -> IO ()

-- The problem you need to solve is looking up a part and passing that part as Maybe Html to insertSnippet. 
-- Here’s an example of fetching a RobotPart from your partsDB.

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- Because Maybe is a Functor, you can use <$> to transform your RobotPart into HTML while remaining in a Maybe.
partHtml :: Maybe Html 
partHtml = renderHtml <$> partVal

-- You can now pass partHtml to insertSnippet easily because of Functor.


-- Converting a list of RobotParts to a list of HTML

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- List is also an instance of Functor. In fact, fmap for a List is the regular map function 

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- Because <$> is just fmap, and for lists fmap is just map, this code is identical to the following.

allPartsHtml' :: [Html]
allPartsHtml' = map renderHtml allParts

-- For lists, it’s more common to use map over <$>, but it’s important to realize these are identical. 
-- One way to think of the Functor type class is as “things that can be mapped over.”

-- Converting a Map of RobotParts to HTML

-- Because Map is an instance of Functor, you can do this easily.

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- Now you can see that you’ve transformed your Map of RobotParts into a Map of Html snippets!

-- GHCi> Map.lookup 1 htmlPartsDB
-- Just "<h2>left arm</h2><p><h3>desc</h3>left ...


-- . Functor for Map is concerned only about the Map’s values and not its keys. 
-- When Map is made an instance of Functor, you’re concerned only about a single type variable, the one used for its values

-- Transforming an IO RobotPart into IO Html

-- Finally, you might have a RobotPart that comes from IO. You’ll simulate this by using return to create an IO type of a RobotPart.

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

-- Suppose you want to turn this into HTML so that you can write the HTML snippet to a file. By now, the pattern should start to be familiar.

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


-- As you can see, Functor’s <$> provides a common interface to apply any function to a value in a context. 
-- For types such as List and Map, this is a convenient way to update values in these containers. 
-- For IO, it’s essential to be able to change values in an IO context, because you can’t take IO values out of their context.


-- In this lesson, our objective was to introduce you to the Functor type class. 
-- The Functor type class allows you to apply an ordinary function to values inside a container (for example, List) or a context (for example, IO or Maybe). 


data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box val) = Box (func val)

unwrap :: Box a -> a
unwrap (Box val) = val

wrap :: a -> Box a
wrap val = Box val

-- GHCi> wrapped = fmap wrap myBox
-- GHCi> wrapped
-- Box (Box 1)
-- GHCi> fmap unwrap wrapped
-- Box 1