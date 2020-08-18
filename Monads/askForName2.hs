
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
   name <- getLine
   let statement = helloPerson name
   putStrLn statement


-- let x = 2
-- f = (x + 1)
-- GHCI>> f x

-- this could be rewritten with lambda expressions as follows:

-- GHCI>> (\x -> x + 1) 2


-- Asa se intampla si mai jos: 

--   let statement = helloPerson name
-- se rescrie cu lambda expression asa:

--   (\statement -> 
--             putStrLn statement) (helloPerson name)



--The above function written with monadic operators
main' :: IO ()
main' = getLine >>=
    (\name -> 
        (\statement -> 
            putStrLn statement) (helloPerson name))

