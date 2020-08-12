-- Building probability tables

-- ou’d like to create probability tables for events and have an easy way to combine them. 
-- You’ll start by looking at a simple table for a coin toss. 
-- You have only two events: getting heads or getting tails.Table 17.1. Probability of heads or tails


Event                   Probability

Heads	                  0.5
Tails	                  0.5


-- You have a list of Strings representing events and a list of Doubles representing probabilities.

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable 
createPTable events probs = PTable events normalizedProbs
   where totalProbs = sum probs
         normalizedProbs = map (\x -> x/totalProbs) probs



showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|",show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
      where pairs = zipWith showPair events probs

-- In GHCi, you can see that you have the basic setup you need:

-- GHCi> createPTable ["heads","tails"] [0.5,0.5]
-- heads|0.5
-- tails|0.5


-- What you want to be able to model using the Monoid type class is the combination of two (or more) PTables. 
-- For example, if you have two coins, you want an outcome like this:

-- heads-heads|0.25
-- heads-tails|0.25
-- tails-heads|0.25
-- tails-tails|0.25


-- This requires generating a combination of all events and all probabilities. 
-- This is called the Cartesian product. You’ll start with a generic way to combine the Cartesian product of two lists with a function. 
-- The cartCombine function takes three arguments: a function for combining the two lists, and two lists.

cardCombine func l1 l2 = zipWith func newL1 cycledL2
   where nToAdd = length l2
         repeatedL1 = map (take nToAdd . repeat) l1 
         newL1 = mconcat repeatedL1
         cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2 -- When combining events, you hyphenate the event names.
 where combiner = (\x y -> mconcat [x,"-",y])          

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2   -- To combine probabilities, you multiply them.


-- With your combineEvent and combineProbs, you can now make PTable an instance of Semigroup.

instance Semigroup PTable where
   (<>) ptable1 (PTable [] []) = ptable1               1
   (<>) (PTable [] []) ptable2 = ptable2
   (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
     where newEvents = combineEvents e1 e2
           newProbs = combineProbs p1 p2


-- Finally, you can implement the Monoid type class. For this class, you know that mappend and <> are the same. 
-- All you need to do is determine the identity, mempty element. 
-- In this case, it’s PTable [] []. Here’s your instance of Monoid for PTable.

instance Monoid PTable where
   mempty = PTable [] []
   mappend = (<>)


-- To see how all this works, let’s see how to create two PTables. 
-- The first is a fair coin, and the other is a color spinner with different probabilities for each spinner.

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- If you want to know the probability of getting tails on the coin and blue on the spinner, you can use your <> operator:

-- GHCi> coin <> spinner
-- heads-red|5.0e-2
-- heads-blue|0.1
-- heads-green|0.35
-- tails-red|5.0e-2
-- tails-blue|0.1
-- tails-green|0.35


-- For your output, you can see that there’s a 0.1, or 10%, probability of flipping tails and spinning blue.

-- What about the probability of flipping heads three times in a row? You can use mconcat to make this easier:

-- GHCi> mconcat [coin,coin,coin]
-- heads-heads-heads|0.125
-- heads-heads-tails|0.125
-- heads-tails-heads|0.125
-- heads-tails-tails|0.125
-- tails-heads-heads|0.125
-- tails-heads-tails|0.125
-- tails-tails-heads|0.125
-- tails-tails-tails|0.125


-- Initially, the idea of abstracting out “combining things” might seem a bit too abstract. 
-- Once you start seeing problems in terms of monoids, it’s remarkable how frequently they appear every day.



-- In this lesson, our objective was to introduce you to two interesting type classes in Haskell: Semigroup and Monoid. 
--- Though both classes have rather strange names, they provide a relatively simple role. 
-- Monoid and Semigroup allow you to combine two instances of a type into a new instance.
--  This idea of abstraction through composition is an important one in Haskell. 
-- The only difference between Monoid and Semigroup is that Monoid requires you to specify an identity element. 
-- Monoid and Semigroup are also a great introduction to the abstract thinking typically involved in more-advanced type classes. 
-- Here you start to see the philosophical difference between type classes in Haskell and interfaces in most OOP languages.




-- Your current implementation of Color doesn’t contain an identity element. 
-- Modify the code in this unit so that Color does have an identity element, and then make Color an instance of Monoid.

data Color = Red |
   Yellow |
   Blue |
   Green |
   Purple |
   Orange |
   Brown |
   Clear deriving (Show,Eq)

instance Semigroup Color where
   (<>) Clear any = any
   (<>) any Clear = any
   (<>) Red Blue = Purple
   (<>) Blue Red = Purple
   (<>) Yellow Blue = Green
   (<>) Blue Yellow = Green
   (<>) Yellow Red = Orange
   (<>) Red Yellow = Orange
   (<>) a b | a == b = a
            | all ('elem' [Red,Blue,Purple]) [a,b] = Purple
            | all ('elem' [Blue,Yellow,Green]) [a,b] = Green
            | all ('elem' [Red,Yellow,Orange]) [a,b] = Orange
            | otherwise = Brown

instance Monoid Color where
   mempty = Clear -- identity value
   mappend col1 col2 = col1 <> col2 