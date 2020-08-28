echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo

--or

-- echo :: IO ()
-- echo = getLine >>= putStrLn

-- main :: IO ()
-- main = do
--    text <- getLine
--    putStrLn text

-- do x <- f; g x is sugar for f >>= (\x -> g x)