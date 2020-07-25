add4 a b c d = a + b + c + d

-- What happens if you call add4 with fewer than four arguments? 
-- This answer seems obvious: it should throw an error. 
-- This isn’t what Haskell does. You can define a mystery value in GHCi by using Add4 and one argument:

mystery = add4 3

-- If you run this code, you’ll find that it doesn’t cause an error. Haskell has created a brand new function for you:

-- GHCi> mystery 2 3 4
-- 12
-- GHCi> mystery 5 6 7
-- 21

-- This mystery function adds 3 to the three remaining arguments you pass to it
-- When you call any function with fewer than the required number of parameters in Haskell, you get a new function that’s waiting for the remaining parameters. 
-- This language feature is called PARTIAL APPLICATION.

 anotherMystery = add4 2 3

-- GHCi> anotherMystery 1 2
-- 8
-- GHCi> anotherMystery 4 5
-- 14


