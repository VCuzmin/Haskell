-- Use a list comprehension that generates a list of correct calendar dates, given that you know the number of days in each month. 
-- For example, it should start with 1 .. 31 for January and be followed by 1 .. 28 for February.
-- Translate the preceding question into do-notation, and then into Monad methods and lambdas.

monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

-- with list comprehensions
dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

-- with do-notation
datesDo :: [Int] -> [Int]
datesDo ends = do
    end <- ends
    date <- [1 .. end]
    return date


-- with monadic operators
datesMonad :: [Int] -> [Int]
datesMonad ends = ends >>= 
    (\end -> 
        [1 .. end] >>= 
            (\date -> return date))