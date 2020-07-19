-- This mathematical model for programming has a variety of practical implications. Because of the simple rules that all functions must take and return values,
--  and must always return the same value for the same argument, Haskell is a safe programming language. 
-- Programs are safe when they always behave exactly the way you expect them to and you can easily reason about their behavior. 
-- A safe programming language is one that forces your programs to behave as expected.

-- Let’s look at code that isn’t safe and violates our simple rules for functions. 
-- Suppose you’re reading through a new code base and you come across lines of code that look like the following.

 -- Hidden state in function calls
tick()
if(timeToReset){
  reset()
}

-- This code clearly isn’t Haskell, because both tick and reset violate the rules we established.
-- Neither function takes any arguments nor returns any value. 
-- The question is, then, what are these functions doing, and how is this different from functions in Haskell?
-- It’s not a long shot to suppose that tick is incrementing a counter and that reset restores that counter to its starting value. 
-- Even if we’re not exactly right, this reasoning gives us insight into our question. 
-- If you aren’t passing an argument to a function, you must be accessing a value in your environment, and if you aren’t returning a value, you must also be changing a value in your environment.
-- When you change a value in your programming environment, you’re changing the program’s state.
-- Changing state creates side effects in your code, and these side effects can make code hard to reason about and therefore unsafe.