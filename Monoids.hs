--  Data.Monoid: a Semigroup with an identity value.

-- Any datatype a which has an associative binary operation will be able to become a member of the Semigroup typeclass. 
-- An instance of Monoid a automatically satisfies the requirements of a Semigroup making Semigroup a strict superset of Monoid. 
-- The Monoid typeclass however does not enforce it's instances to already be instances of Semigroup

-- Declaration

class Semigroup m => Monoid m where
  mempty :: m

  -- defining mappend is unnecessary, it copies from Semigroup
  mappend :: m -> m -> m
  mappend = (<>)

  -- defining mconcat is optional, since it has the following default:
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty


-- together with the following laws:

-- Identity laws
x <> mempty = x
mempty <> x = x

-- Associativity
(x <> y) <> z = x <> (y <> z)

-- Examples
-- The prototypical and perhaps most important example is lists, which form a monoid under concatenation:

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []


-- Another type class that’s similar to Semigroup is Monoid. 
-- The only major difference between Semigroup and Monoid is that Monoid requires an identity element for the type. 
-- An identity element means that x <> id = x (and id <> x = x). 
-- So for addition of integers, the identity element would be 0. 
-- But in its current state, your Color type doesn’t have an identity element. 
-- Having an identity element might seem like a small detail, but it greatly increases the power of a type by allowing you to use a fold function to easily combine lists of the same type.

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

-- Why mempty instead of identity? Why mappened instead of <>? 
-- These oddities in naming occur because the Monoid type class was added to Haskell before Semigroup. 
-- The most common Monoid is a list. The empty list is the identity for lists, and ++ (the append operator) is the <> operator for lists. 
-- The strange names of Monoid’s methods are just m (for Monoid) tacked onto common list functions: empty, append, and concat. 
-- Here you can compare all three ways to do the same identity operation on a list:

GHCi> [1,2,3] ++ []
[1,2,3]

GHCi> [1,2,3] <> []
[1,2,3]

GHCi> [1,2,3] `mappend` mempty
[1,2,3]

-- Notice that mappend has the exact same type signature as <>.

--  mconcat: Combining multiple Monoids at once

-- The easiest way to see how powerful identity is, is to explore the final method in the definition of Monoid: mconcat. 
-- The only required definitions in Monoid are mempty and mappend. If you implement these two, you get mconcat for free. 
-- If you look at the type signature of mconcat, you get a good sense of what it does:

-- mconcat :: Monoid a => [a] -> a
-- The mconcat method takes a list of Monoids and combines them, returning a single Monoid. 
-- The best way to understand mconcat is by taking a list of lists and seeing what happens when you apply mconcat. 
-- To make things easier, you’ll use strings because those are just lists of Chars:

-- GHCi> mconcat ["does"," this"," make"," sense?"]
-- "does this make sense?"

-- The great thing about mconcat is that because you’ve defined mempty and mappend, Haskell can automatically infer mconcat! 
-- This is because the definition of mconcat relies only on foldr (lesson 9), mappend, and mempty. 
-- Here’s the definition of mconcat:

-- mconcat = foldr mappend mempty

-- Monoid laws
-- Just like Semigroup, there are Monoid type class laws. There are four:

-- The first is that mappend mempty x is x. Remembering that mappend is the same as (++), and mempty is [] for lists, this intuitively means that
-- []  ++ [1,2,3] = [1,2,3]
-- The second is just the first with the order reversed: mappend x mempty is x. In list form this is
-- [1,2,3] ++ [] = [1,2,3]
-- The third is that mappend x (mappend y z) = mappend (mappend x y) z. This is just associativity, and again for lists this seems rather obvious:
-- [1] ++ ([2] ++ [3]) = ([1] ++ [2]) ++ [3]
-- Because this is a Semigroup law, then if mappend is already implemented as <>, this law can be assumed because it’s required by the Semigroup laws.
-- The fourth is just our definition of mconcat:
-- mconcat = foldr mappend mempty