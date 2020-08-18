-- ! Making Monads easier with do-notation

-- The Monad type class allows for powerful abstraction when using types in context. 
-- But the use of the Monad methods >>=, >>, and return quickly becomes cumbersome.

-- In the preceding lesson, you left off with a helloName IO action that asks the user for their name and then says hello to them.
-- ! see askForName.hs file

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name ->
              return (nameStatement name)) >>=
            putStrLn


-- You were able to achieve this by using the methods of the Monad type class. As a refresher, here are those methods:

-- >> allows you to perform an IO action and chain it with another action, ignoring its value.
-- >>= allows you to perform an IO action and then hand off the return value of that function to another waiting for a value.
-- (\x -> return (func x)) allows you to take an ordinary function and have it work in the context of IO.

-- Unfortunately, this code is messy, and is difficult to read and write. 
-- Thankfully, Haskell has a great solution to this problem!

-- ! DO-NOTATION

-- It turns out that you’ve already seen the solution to making your monadic code look cleaner: do-notation. 
-- Do-notation is syntactic sugar for using >>, >>=, and (\x -> return (func x)). 
-- Here’s the previous example rewritten in do-notation.

--  Rewriting helloName using do-notation
helloNameDo :: IO ()
helloNameDo = do
   askForName
   name <- getLine
   putStrLn (nameStatement name)


--or another example

-- ! see askForName2 .hs 

echo :: IO ()
echo = getLine >>= 
    (\val -> putStrLn val)


-- or with point-free notation:
echo' :: IO ()
echo' = getLine >>= putStrLn

-- Rewrite echo by using do-notation.

echo'' :: IO ()
echo'' = do
 val <- getLine
 putStrLn val



--  ! If you’re having trouble understanding the translation of let and <- into lambda expressions, 
-- ! it’d be a good idea to see askForName2.hs file and explanation from there
-- ! For reasons that should be clear, do-notation is strongly preferred for nontrivial use of monadic operators