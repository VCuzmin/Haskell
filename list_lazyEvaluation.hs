GHCi> [1 .. 10] 
[1,2,3,4,5,6,7,8,9,10]

GHCi> [1,3 .. 10]
[1,3,5,7,9]

GHCi> [1, 1.5 .. 5]
[1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]

GHCi> [1,0 .. -10]
[1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]


-- What happens if you forget to put an upper bound to your range?

GHCi> [1 .. ]
[1,2,3,4,5,6,7,8,9,10,11,12 ..

-- An unending list is generated! This is cool but quickly clogs up the terminal and doesn’t seem particularly useful. 
-- What’s interesting is that you can assign this list to a variable and even use it in a function:

simple x = x
longList = [1 .. ]
stillLongList = simple longList

-- What’s shocking is that this code compiles just fine. 
-- You defined an infinite list and then used it in a function. 
-- Why didn’t Haskell get stuck trying to evaluate an infinitely long list? 
-- Haskell uses a special form of evaluation called lazy evaluation. 
-- In lazy evaluation, no code is evaluated until it’s needed. In the case of longList, none of the values in the list were needed for computation.


backwardsInfinity = reverse [1..].

-- Even though you’re reversing an infinite list, you’re never calling this code, so the infinite list is never evaluated. 
-- If you loaded this code into GHCi and typed the following
GHCi> backwardsInfinity 
-- you’d have a problem, as the program would need to evaluate this argument to print it out.