minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
   putStrLn "Enter three numbers"
   minInt <- minOfInts
   putStrLn (show minInt ++ " is the smallest")


--    Now you can compile and run your min3.hs:

-- $ ghc min3.hs
-- $ ./min3.hs
-- Enter three numbers
-- 1
-- 2
-- 3

-- 1 is the smallest