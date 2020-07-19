-- The simple function takes a single argument x and then returns this argument untouched. 
-- Notice that unlike many other programming languages, in Haskell you don’t need to specify that you’re returning a value. 
-- In Haskell, functions must return a value, so there’s never a need to make this explicit.
--  You can load your simple function into GHCi and see how it behaves.
-- To load a function, all you have to do is have it in a file and use :load <filename> in GHCi:

-- GHCi> simple 2
-- 2
-- GHCi> simple "dog"
-- "dog"

simple x = x

-- In this section, we’re using GHCi—Haskell’s Interactive Read-Eval-Print Loop (REPL)—to run commands and get results.

-- All functions in Haskell follow three rules that force them to behave like functions in math:

-- All functions must take an argument.
-- All functions must return a value.
-- Anytime a function is called with the same argument, it must return the same value.

-- The third rule is part of the basic mathematical definition of a function. 
-- When the rule that the same argument must always produce the same result is applied to function in a programming language,
--  it’s called referential transparency.