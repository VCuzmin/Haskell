echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String an we'll echo it!" >>
              getLine >>= putStrLn

main :: IO ()
main = echoVerbose