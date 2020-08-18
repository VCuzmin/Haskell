askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> -- nu ma intereseaza rezultatul valorii lui askForName. 
-- Ma intereseaza doar sideefectul pe care il face
            getLine >>= 
            (\name -> 
                return (nameStatement name)) >>=
            putStrLn
