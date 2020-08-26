import qualified Data.Map as Map


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



--  ! USING DO-NOTATION TO REUSE THE SAME CODE IN DIFFERENT CONTEXTS

-- The problem setup

-- To get started, you need to model your candidate data. 
-- Each Candidate is tracked by a unique ID. During the interview, candidates are given a code review and a culture fit interview. 
-- Each of these is scored by using a grade.

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show

  --  The big thing you want to do is determine whether a candidate is viable. 
  -- If a candidate is viable, you’ll pass that person on to a committee for review. 
  -- Here’s your code for viable, which makes sure a candidate passes your minimum requirements.

viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding
                 ,passedCultureFit
                 ,educationMin]

testCandidate :: Candidate
testCandidate = Candidate
   { candidateId = 1
   , codeReview = A
   , cultureFit = A
   , education = PhD }

-- GHCi> viable testCandidate
-- True

-- Next you’ll look at three contexts in which you might want to check whether a candidate is viable.

-- ! The IO context—building a command-line tool
-- Your first case is building a command-line tool so that someone can manually enter in the data about a candidate.
-- The first thing you need is a bunch of simple IO actions to read in Int, Grade, and Degree types. 
-- You could use do-notation to implement these actions, but this is a great example of when using >>= comes in handy. 
-- Each of these actions needs a way to connect getLine with reading the result, and finally returning that result back as an IO type.

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)


readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })


assessCandidateIO :: IO String
assessCandidateIO = do
   candidate <- readCandidate
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

--   You could put this in a main, compile your program, and run it, but in this case it’s easier to use GHCi:

-- GHCi> assessCandidateIO
-- enter id:
-- 1
-- enter code grade:
-- A
-- enter culture fit grade:
-- B
-- enter education:
-- PhD
-- "passed"



-- ! Rewrite readGrade with do-notation
-- Answer:

readGradeDo :: IO Grade
readGradeDo = do
   input <- getLine
   return (read input)

-- *Main> readGradeDo
-- A
-- A
-- *Main>



-- !The Maybe context—working with a map of candidates

-- Entering users one by one in the command line is a tedious way to check candidate data. 
-- In our next example, you’ll use Data.Map to store a bunch of candidates and then look them up. 
-- First you need a few candidates to work with.

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }



candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                           ,(2,candidate2)
                           ,(3,candidate3)]


-- Once again you want to assess your candidates and return a string if you’ve found them. 
-- Now you can use your candidateDB. Because each lookup will return a Maybe type, you have a problem in a different context than the IO case before. 
-- In the last example, you were worried about interacting with a user; now you’re concerned with passing around potentially missing values. 
-- To handle this, you need a function that looks a lot like assessCandidateIO but works for Maybe types.

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
   candidate <- Map.lookup cId candidateDB
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

--  Now all you have to do is pass in a potential candidate’s ID and you’ll get your result in a Maybe context:

-- GHCi> assessCandidateMaybe 1
-- Just "failed"
-- GHCi> assessCandidateMaybe 3
-- Just "passed"
-- GHCi> assessCandidateMaybe 4
-- Nothing


-- ! The List context—processing a list of candidates
-- A list of possible candidates
candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

-- Because List is an instance of Monad, you should be able to convert your other assess-Candidate function into an assessCandidateList function. 
-- If you do and pass in a list, you get a useful result.


viable' :: Candidate -> [String]
viable' candidate = if (viable candidate)
  then ["passed"]
  else ["failed"]   

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement


-- GHCi> assessCandidateList candidates
-- ["failed","failed","passed"]

--or 

-- GHCi> [candidate1, candidate2, candidate3] >>= viable'
-- ["failed","failed","passed"]




-- ! Putting it all together and writing a monadic function

-- So far, you’ve focused primarily on the way that do-notation and the Monad type class allow you to solve problems while abstracting away the context:

-- You can write code for IO types and not worry about the mismatch between IO Strings and regular Strings.
-- You can write code for Maybe and forget about dealing with missing values.
-- You can even write code for lists and pretend you have only a single value.

-- But there’s another benefit to Monad that can emerge as a consequence of letting you forget context when you write programs. 
-- The action and two functions you wrote—assessCandidateIO, assessCandiateMaybe, and assessCandidateList—all share nearly identical code. 
-- Not only is it easier to solve a problem in a specific context with the Monad type class, but you end up with a single solution that works in any context.

-- The only limitation to using the same code in all three contexts is that the type signatures are too restrictive. 
-- Because IO, Maybe, and List are all instances of Monad, you can use a type class constraint in your definition of a universal assessCandidate function. 
-- The amazing thing here is you need to change only the type signature of your assessCandidateList function to do this.

-- The monadic assessCandidate works on IO, Maybe, and List
assessCandidate :: Monad m =>  m Candidate -> m String
assessCandidate candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

-- In GHCi, you can now demonstrate by using this single function in three contexts:

-- GHCi> assessCandidate readCandidate
-- enter id:
-- 1
-- enter code grade:
-- A
-- enter culture fit grade:
-- B
-- enter education:
-- PhD
-- "passed"

-- GHCi> assessCandidate (Map.lookup 1 candidateDB)
-- Just "failed"
-- GHCi> assessCandidate (Map.lookup 2 candidateDB)
-- Just "failed"
-- GHCi> assessCandidate (Map.lookup 3 candidateDB)
-- Just "passed"

-- GHCi> assessCandidate candidates
-- ["failed","failed","passed"]