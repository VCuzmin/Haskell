-- ! Combining two Map lookups

-- In this section, you’ll explore a common issue of needing to look up a value in one Map in order to access another value in a second Map. 
-- This can happen anytime you need one value to look up another, such as the following:

-- Looking up a zip code to find a city, and then looking up the city to find the state
-- Using an employee name to look up an employee ID, and then using the ID to look up a record

-- You’re writing code for managing user credits for a mobile gaming platform. 
-- Currently, each user is identified as a unique GamerId that’s just an Int. 
-- Suppose that earlier instances of your program used a unique Username, a String, to associate a user with the credits in their account. 
-- Because of this legacy dependence of Username as an identity, to look up user credits on newer users, you still have to look up a Username given a GamerId and then use the Username to look up the credits in their account. 
-- Here’s the basic code to get you started.

import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName                      
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits                 
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

-- With your sample data in place, you can start working on your main problem: writing a function to look up a user’s credits given that user’s GamerId. 
--You want a function that will look up PlayerCredits given a GamerId. 
-- You still want your PlayerCredits value to be a Maybe PlayerCredits because it’s entirely possible that either you have a missing GamerId or there’s a missing entry for your GamerID in creditsDB. 
-- The function you want is as follows.

-- creditsFromId :: GamerId -> Maybe PlayerCredits


-- To create this function, you have to combine two Map.lookup functions. 
-- You’ll create helper functions that abstract out your databases. 
-- The lookupUserName function will take a GamerID and give you a Maybe UserName result, and the lookupCredits function will take a UserName and give the user a Maybe Credits result.

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- Before you dive deeper, you should think about the type signature of the missing function that you need. 
-- You need to connect the result of lookupUserName, Maybe Username, with the function lookupCredits, UserName -> Maybe PlayerCredits. 
-- For this concrete case, the type signature of your function is as follows:

-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

-- The general form of the combining function you want is as follows:

-- Applicative f => f a -> (a -> f b) -> f b

-- You’ll assume the Applicative constraint rather than Functor only because Applicative is more powerful. 
-- If you can’t solve your problem with Applicative, you can’t solve it with Functor either. 
-- Now let’s take a look at the tools you get from Applicative and Functor:

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- pure :: Applicative f => a -> f a

-- Unfortunately, for all the power you’ve gained with Applicative, it doesn’t look like any combination of these tools will solve this rather straightforward problem of wanting to chain together two functions. 
-- You can solve this problem by writing a wrapper for lookup-Credits to be a function of Maybe UserName -> Maybe PlayerCredits.

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

-- Now you can put together your final creditsFromId function

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- GHCi> creditsFromId 1
-- Just 2000
-- GHCi> creditsFromId 100
-- Nothing

-- This solution works, but having to write a wrapper function to make it work for Maybe should be a warning at this point. 


-- ! Writing a not-so-trivial echo IO action
-- The reason your problem with Maybe doesn’t seem so bad is that Maybe is an easy context to work in. 
-- You can always manually create a solution to any Maybe problem by a clever use of pattern matching on Just and Nothing. 
-- The IO type, on the other hand, isn’t nearly as friendly. 
-- To demonstrate this, let’s attempt to solve an easy-looking problem. 
-- You want to write a simple IO action, echo. The echo action is a single action that reads in user input and immediately prints it back. 
-- To do this, you need to combine two IO actions that you already know well:

getLine :: IO String
putStrLn :: String -> IO ()

-- And of course the type of echo is as follows:

echo :: IO ()

-- You need to combine getLine with putStrLn. 
-- If you once again think of this problem in types, you’ll see a familiar pattern. 
-- You need a function that combines getLine and putStrln and returns an IO String:

-- IO String -> (String -> IO ()) -> IO ()

-- If you abstract this out, you have this:

-- Applicative f => f a -> (a -> f b) -> f b
-- This is exactly the same type signature you ended up with before. 
-- To solve this problem, you need something strictly more powerful than either Functor or Applicative. 
-- This finally brings you to the Monad type class!


-- Why can’t you write a function like creditsFromId to solve this problem (With IO)?

-- Because you have no way of getting a value out of an IO context as you do a Maybe context. 
-- You need more powerful tools such as Applicative and Functor to work with IO types.

-- ! THE BIND OPERATOR: >>=
-- The missing operator you need is >>= (pronounced bind) and has the following type signature:

(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- As you can see, (>>=) has exactly the signature you were looking for! From the type class constraint, you can see that >>= is a member of the Monad type class. 
-- Maybe and IO are both instances of Monad, which means you can use >>= to solve your problems. 
-- With bind, you can find a more elegant solution to your creditFromId function.

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId = lookupUserName id >>= lookupCredits 


-- As you can see, >>= allows you to chain together a function of a type (a -> m b). 
-- In the case of Maybe, this means you can endlessly chain together lookups. 
-- For example, suppose you have yet another level of indirection. 
-- Imagine your mobile gaming company was purchased by WillCo Industries, and now each GamerId is itself associated with a WillCoId.

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

-- Now you need a new function, creditsFromWCId, of type WillCoId -> Maybe PlayerCredits. 
-- You can easily create this by chaining all three of your lookup functions with >>=.

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

-- In GHCi, you can see that this works as expected:

-- GHCi> creditsFromWCId 1001
-- Just 2000
-- GHCi> creditsFromWCId 100
-- Nothing


-- Although using >>= made chaining together Maybe functions much easier, it’s essential to solving your IO action problem. 
-- When you left off, you wanted to chain together getLine and putStrLn. 
-- But you were absolutely stuck because there was no way to combine these actions and there was no way to crack open the IO type to write a wrapper as you did for Maybe. 
-- With >>=, creating an echo function is trivially easy. 
-- Let’s put what you know into an echo.hs file and see how it behaves.

-- ! see echo.hs and echo.exe files

-- If you compile this program, you can see that it behaves as expected:

-- $ ghc echo.hs
-- $ ./echo
-- Hello World!
-- Hello World!

-- The >>= operator is the heart of the Monad type class. 
-- Though relatively simple, the >>= operator is the final piece in your puzzle. 
-- Now that you have <$>, <*>, pure, and >>=, you can chain together any computation you need in a context.



-- Combine readInt and printDouble (defined next) into a single IO action:
-- ! see orintDouble.hs and printDouble.exe files



-- ! THE MONAD TYPE CLASS
-- In the same way the Applicative type class extends the power of Functor, the Monad type class extends the power of Applicative. 

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a

-- Here you have four important methods in your type class definition. 
-- The only method required for the minimum definition of Monad is >>=. 
-- You’ve already seen how >>= lets you chain together a sequence of functions that put a normal value into a context. 
-- The fail method handles the case of errors happening in your Monad. 
-- For Maybe, fail returns Nothing; and for IO, fail raises an I/O error. 
-- You’ll discuss fail in more depth in unit 7 when we discuss handling errors in Haskell. 
-- That leaves only >> and return to explain.

-- The return method should look awfully familiar. If you compare this to pure, you’ll find they’re nearly identical:

-- pure :: Applicative f => a -> f a
-- return :: Monad m => a -> m a

-- The only difference is that pure has a type class restraint on Applicative, whereas return has a constraint on the Monad type class. 
-- It turns out that pure and return are identical and have different names only for historical reasons. 
-- The Monad type class predates the Applicative type class, so the return method exists for legacy reasons. 
-- Because every Monad must be an Applicative, it would be reasonable to use pure over return because pure will work everywhere return does. 
-- But this isn’t typically the case. 
-- When using pure in the context of Monad, it’s preferable to stick with return.

-- Finally, you can look at the >> operator. If you look carefully, >> has a rather strange type signature

>> :: ma -> mb -> ma

-- It looks like this operator throws away the first m a type. 
-- It turns out this is exactly what >> does. 
-- Why would you want this? It’s particularly useful in contexts that produce side effects such as IO (there are others, which we’ll discuss in unit 7). 
-- So far, the only context you’ve seen like this is IO. Whenever you use putStrLn, you don’t get anything back. 
-- It’s common that you’ll want to print something to the user and just throw away the IO () result. 
-- For example, you might want to modify your echo.hs program so that it lets your user know what it’s doing.

-- ! see echoVerbose.hs file

-- When working with IO, >> is useful anytime you need to perform an IO action that doesn’t meaningfully return a value.

--  Using Monad to build a Hello <Name> program

-- To demonstrate how you tie all these together, let’s write a simple IO action that will ask a user’s name, and then print out "Hello, <NAME>!". 
-- You need to chain together a few basic functions to do this. 
-- The first is an IO action that will ask for the name; this is simply putStrLn with your question.

askForName :: IO ()
askForName = putStrLn "What is your name?"

-- The next IO action you need to use is getLine. 
-- After that, you need to take the result of getLine and make your "Hello, <NAME>!" string. 
-- This function is a regular function of the form String -> String.

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- Then you have to send the results of this to putStrLn, and your action is finished. 
-- You start with chaining together askForName and getLine with >>, because you don’t need the results:

-- (askForName >> getLine)

-- The next part is tricky; you now have an IO String, but you need to connect it with name-Statement, which is a regular String -> String function. 
-- You can use >>= to do this if you can make nameStatement return an IO String. 
 -- You could rewrite nameStatement, but a more common solution is to wrap nameStatement in a lambda and use return at the end. 
-- Because of type inference, Haskell knows which context to put the type into


-- Turn (+ 2) from type Num a => a -> a to type Num a => a -> IO a using a lambda and return. 
-- Use :t in GHCi to double-check that you’re getting the correct type.
-- ? Answer: 
-- (\n -> return ((+ 2) n))


-- This is your program so far:

-- (askForName >> getLine) >>= (\name -> return (nameStatement name))

-- To finish, you use >>= to return the results to putStrLn. Here’s your final helloName IO action.

-- ! see askForName.hs file

-- You can either make this its own program or use GHCi to test it out. Here’s the result in GHCi:

-- GHCi> helloName
-- What is your name?
-- Will
-- Hello, Will!