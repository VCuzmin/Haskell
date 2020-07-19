x = 2

-- The only catch with variables in Haskell is that they’re not really variable at all! 
-- If you were to try to compile the following bit of Haskell, you’d get an error, as shown in the next listing.

x = 2
x = 3 -- Won’t compile because it changes the value of x

-- When working in GHCi, you’re allowed to reassign variables. Here’s an example:

-- GHCi> x = 7
-- GHCi> x
-- 7
-- GHCi> x  = [1,2,3]
-- GHCi> x
-- [1,2,3]

-- The inability to change variables is also related to referential transparency. 
-- This may seem like a strict rule to follow, but the reward is that you always know that after calling a function, the world remains the same.

-- The key benefit of variables in programming is to clarify your code and avoid repetition. 
-- For example, suppose you want a function called calcChange. 
-- This function takes two arguments: how much is owed and how much is given. 
-- If you’re given enough money, you return the difference. 
-- But if you aren’t given enough money, you don’t want to give negative dollars; you’ll return 0. Here’s one way to write this.

calcChange owed given = if given - owed > 0
                        then given - owed 
                        else 0

-- Two things are wrong with this function:

-- Even for a tiny function, it’s hard to read. Each time you see the expression given - owed, you have to reason about what’s happening. For anything more complicated than subtraction, this would be unpleasant.
-- You’re repeating your computation! Subtraction is a cheap operation, but if this had been a costlier operation, you’d be needlessly wasting resources.
-- Haskell solves these problems by using a special where clause. Here’s the previous function written with a where clause.

calcChange owed given = if change > 0
                        then change
                        else 0
        where change = owed - given --given – owed is computed only once and assigned to change.


doublePlusTwo x = doubleX + 2
   where doubleX = x * 2