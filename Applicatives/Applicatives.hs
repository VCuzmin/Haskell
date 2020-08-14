--  The Applicative type class extends the power of Functor by allowing you to use functions that are themselves in a context.

-- Although this may not seem useful, it allows you to chain together long sequences of computation in a context such as IO or Maybe.
-- In your first example, you’ll see the limitations of Functor by building a command-line tool that allows the user to calculate the distance between two cities. 
-- The issue is that you need to pass two Maybe values to a function, which surprisingly Functor can’t do. You’ll then see how Applicative resolves this issue. 
-- After you learn about Applicative, you’ll see how this can help you create data in the context of either IO or Maybe, while allowing you to reuse the majority of your code.

-- ! A COMMAND-LINE APPLICATION FOR CALCULATING THE DISTANCE BETWEEN CITIES

import qualified Data.Map as Map

-- Let’s assume you have a Map type for locations on the globe and their latitude and longitude as a tuple.

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]



-- What you’d like to do is calculate the distance between two points on the globe from your locationDB. 
-- To do this, you need to use the formula for calculating distance on a globe. Because a globe curves, you can’t calculate the straight-line distance between two points. 
-- Instead, you need to use the Haversine formula. Note that you need to convert your latitude and longitude to radians first. 
-- Here’s an implementation of haversine (you don’t need to understand the details of this function).

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
 where rlat = toRadians lat
       rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where (rlat1,rlong1) = latLongToRads coords1
       (rlat2,rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1
       a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
       c = 2 * atan2 (sqrt a) (sqrt (1-a))
       earthRadius = 3961.0


-- Here’s an example of using haversine to compute the distance between two points on the globe:

-- GHCi> haversine (40.7776,-73.9691) (42.6054,-70.7829)
-- 207.3909006336738

-- Next you want to make a simple command-line tool that will let the user get the distance between two cities. 
-- You want the user to enter in two city names, and you’ll return the distance. 
-- Given that you’re dealing with user input, you definitely need to handle the case in which the user enters a city that doesn’t exist in your database. 
-- If one of the names is missing, you’ll let the user know that an error occurred in their input.

-- What you want to end up with is an IO action that takes a Maybe value for your distance and either prints the distance or tells the user that an error occurred.

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

-- Now you just have to tie everything together. You need to get two locations from your locationDB, calculate their distance, and then pass that distance to printDistance. 
-- The trouble is that your locationDB will give you Maybe values. 
-- Thinking in types, here’s the problem. You have haversine, which is of this type:

-- haversine :: LatLong -> LatLong -> Double

-- What you need is a function that looks like:

-- Maybe LatLong -> Maybe LatLong -> Maybe Double
-- Maybe LatLong  -- These values comes from using Map.lookup on your locationDB
-- Maybe Double -- You don't want your result to leave the context of the Maybe. You'll let printDistance
-- handle the case of missing cities.

-- This is almost exactly the type signature of haversine, but everything is in the context of a Maybe.
-- The naive solution is to put a wrapper function around haversine, which will work the specific case of Maybe.

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)
-- The haversineMaybe solution is a poor one for two reasons. First, you have to write wrappers for any similar function, which is needlessly repetitive. 
-- Second, you have to write a different version of haversineMaybe for other similar context types such as IO. 
-- Because the promise of the Functor type is to provide a general way of working in different contexts, let’s see if you can solve this problem with Functor.




-- ! The limitations of Functor

-- The fmap function takes any function from type a to type b, and the value of type a in the context of a Functor (like Maybe), and returns a value of type b in the same context. 
-- If you think of the problem in terms of types, this is pretty close. 
-- The major difference is you have one extra argument. What you want to do is this:

-- Take haversine, which is (LatLong -> LatLong -> Double).
-- Take two arguments of type Maybe: Maybe LatLong -> Maybe LatLong.
-- And finally, you want your answer in a Maybe: Maybe Double.
-- This leads to the following series of type transformations:

-- (LatLong -> LatLong -> Double) ->
--       (Maybe LatLong ->  Maybe LatLong -> Maybe Double)


-- If you translate this to a more generic type signature, you get the following:

--  Functor f => (a -> b -> c) -> f a -> f b -> f c

-- This is nearly identical to fmap, except you’re adding one argument. 
-- This is one of the limitations of Functor’s fmap: it only works on single-argument functions. 
-- Because your main problem is having an extra argument, using partial application should move you close to a solution.



-- ! USING <*> FOR PARTIAL APPLICATION IN A CONTEXT

-- The problem you need to solve now is generalizing Functor’s fmap to work with multiple arguments.
-- Multi-argument functions are just a chain of single-argument functions. 
-- The key to solving your problem lies in being able to perform partial application in a context such as Maybe or IO.

-- The real limitation of Functor’s <$> is that if you end up with a function in a context, through partial application, you have no way of using that function. For example, you can use <$>, (+), and the number 1 in a context to create a maybeInc function.

--  Using Functor’s <$> operator for partial application in a context
-- maybeInc = (+) <$> Just 1

-- If you look up the type of this function, you find that it’s as follows:

-- maybeInc :: Maybe (Integer -> Integer)

-- e (+) operator is a function that takes two values; by using <$> on a Maybe value, you created a function waiting for a missing value, but it’s inside a Maybe. 
-- You now have a Maybe function, but there’s no way to apply this function!

-- ! Introducing the <*> operator

-- A powerful type class called Applicative contains a method that’s the <*> operator (pronounced app)
-- Applicative’s <*> allows you to apply a function in a context. Now you can use maybeInc to increment Maybe values. 
-- Here are a couple of examples in GHCi:

-- GHCi> maybeInc <*> Just 5
-- Just 6
-- GHCi> maybeInc <*> Nothing
-- Nothing
-- GHCi> maybeInc <*> Just 100
-- Just 101

-- You’ve not only solved the case of combining two values inside a Maybe, but also found a general way to use existing binary functions in a Maybe context.

-- You can use this to combine Strings in a Maybe context as well:

-- GHCi> (++) <$> Just "cats" <*> Just " and dogs"
-- Just "cats and dogs"
-- GHCi> (++) <$> Nothing <*> Just " and dogs"
-- Nothing
-- GHCi> (++) <$> Just "cats" <*> Nothing
-- Nothing

-- Because of the way partial application works, you can use <$> and <*> to chain together any number of arguments.


-- ! Using <*> to finish your city distance program

-- With Applicative and <*>, you can finally solve your problem of wanting to use your haversine function with two Maybe values:

-- GHCi> startingCity = Map.lookup "Carcosa" locationDB
-- GHCi> destCity = Map.lookup "Innsmouth" locationDB
-- GHCi> haversine <$> startingCity <*> destCity
-- Just 1415.7942372467567

-- Now that you can extend the power of fmap with <*>, you can put everything together to build a program that will take two city names from user input, and output the distance. 
-- Here’s the main for your program. 
-- ! See dist.hs file and dist.exe file


-- ! Using a multi-argument function in IO using <$> and <*>

-- IO is also a member of Applicative. 
-- To show this off, let’s see how to use <$> and <*> to make a simple command-line tool that returns the minimum of three numbers entered by the user, called min3.hs. 
-- You’ll start with a three-argument function called minOfThree, which gives you the minimum of three values.

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

-- Next you’ll create a simple IO action, readInt, which will read an Int from the command line.

readInt :: IO Int
readInt = read <$> getLine

-- Now you can use <$> with <*> to make an IO action that reads in three Ints and returns the minimum.

-- Listing 28.9. minOfInts shows using multiple arguments with <*>
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

-- Finally, you can put this in a main.
-- ! See min.hs file and min.exe file

-- Because of the power of partial application and <*>, you can chain together as many arguments as you’d like!


-- You can use it with the Maybe too

-- GHCi> minOfThree <$> Just 10 <*> Just 3 <*> Just 6
-- Just 3