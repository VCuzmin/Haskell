echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo