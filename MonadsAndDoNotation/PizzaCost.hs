main :: IO ()
main = do
   putStrLn "What is the size of pizza 1"
   size1 <- getLine
   putStrLn "What is the cost of pizza 1"
   cost1 <- getLine
   putStrLn "What is the size of pizza 2"
   size2 <-  getLine
   putStrLn "What is the cost of pizza 2"
   cost2 <- getLine
   let pizza1 = (read size1, read cost1)
   let pizza2 = (read size2, read cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   putStrLn (describePizza betterPizza)


-- Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.

main :: IO ()
main = putStrLn "What is the size of pizza 1" >>
       getLine >>=
       (\size1 ->
         putStrLn "What is the cost of pizza 1" >>
         getLine >>=
         (\cost1 ->
           putStrLn "What is the size of pizza 2" >>
           getLine >>=
           (\size2 ->
             putStrLn "What is the cost of pizza 2" >>
             getLine >>=
             (\cost2 ->
               (\pizza1 ->
                 (\pizza2 ->
                   (\betterPizza ->
                     putStrLn (describePizza betterPizza):
                   ) (comparePizzas pizza1 pizza2)
                 )(read size2,read cost2)
               )(read size1, read cost1)
             ))))