-- read :: String -> Int
-- getLine :: IO String


readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n*2)

main :: IO ()
main = readInt >>= printDouble