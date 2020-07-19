-- The key issue is that this code is in one big monolithic function named messyMain. 
-- The advice that it’s good practice to write modular code is fairly universal in software, 
-- but in Haskell it’s essential for writing code that you can understand and troubleshoot.

messyMain :: IO ()
messyMain = do 
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the author?"
    author <- getLine
    print ("Dear " ++ recipient ++ ",\n" ++
          "Thanks for buying " ++ title ++ "\nthanks, \n" ++ 
          author)


-- Everything works fine, but it’d be much easier to work with if this code was broken up a bit. 
-- Your primary goal is to create an email, but it’s easy to see that the email consists of tying together three parts: 
-- the recipient section, the body, and the signature. You’ll start by pulling out these parts into their own functions.

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart author = "Thanks, \n" ++ author

-- Everything is looking good! Now you need a function to tie it all together.

createEmail recipient bookTitle author = toPart recipient ++ bodyPart bookTitle ++ fromPart author

-- With all your functions written, you can test createEmail:

-- GHCi> createEmail "Happy Reader" "Learn Haskell" "Will Kurt"
-- "Dear Happy Reader,\nThanks for buying Learn Haskell.\nThanks,\nWill Kurt"

-- Your functions each work as expected. Now you can put them all together in your messyMain.

messyMainCleaned :: IO ()
messyMainCleaned = do 
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the author?"
    author <- getLine
    print (createEmail recipient title author)
