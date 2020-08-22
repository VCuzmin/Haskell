-- The $ operator is for avoiding parentheses. Anything appearing after it will take precedence over anything that comes before.

-- For example, let's say you've got a line that reads:

putStrLn (show (1 + 1))

-- If you want to get rid of those parentheses, any of the following lines would also do the same thing:

putStrLn (show $ 1 + 1)
putStrLn $ show (1 + 1)
putStrLn $ show $ 1 + 1

f $ g x becomes f (g x).

f = g . h
becomes

f x = (g . h) x
becomes

f x = g (h x)


-- ($) allows functions to be chained together without adding parentheses to control evaluation order:

Prelude> head (tail "asdf")
's'

Prelude> head $ tail "asdf"
's'