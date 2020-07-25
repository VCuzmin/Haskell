inc n = n+1
double n = n*2
square n = n^2

ifEven func n = if even n
                then func n
                else n

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

-- Using functions as arguments helped to clean up your code. 
-- But you’ll notice you’re still repeating a programming pattern! 
-- Each of these definitions is identical except for the function you’re passing to ifEven. 
-- What you want is a function that builds ifEvenX functions. 
-- To solve this, you can build a new function that returns functions, called genIfEven

-- Now you’re passing in a function and returning a lambda function. 
-- The function f that you passed in is captured inside the lambda function! 
-- When you capture a value inside a lambda function, this is referred to as a CLOSURE.

genIfEven f= (\x -> ifEven f x)

ifEvenInc_v2 = genIfEven inc 
ifEvenDouble_v2 = genIfEven double
ifEvenSquare_v2 = genIfEven square


getRequestURL host apiKey resource id = host ++
                                        "/" ++ 
                                        resource ++
                                        "/" ++ 
                                        id ++
                                        "?token=" ++
                                        apiKey

-- GHCi> getRequestURL "http://example.com" "1337hAsk3ll" "book" "1234"

-- "http://example.com/book/1234?token=1337hAsk3ll"

-- Nearly every programmer on the team will be focusing on data from just a few hosts. 
-- It seems silly, not to mention error-prone, to have programmers manually type in http://example.com every time they need to make a request. 
-- What you need is a function that everyone can use to generate a request URL builder just for them. 

genHostRequestBuilder host = (\apiKey resource id -> 
                               getRequestURL host apiKey resource id)  --you're capturing the host argument in this lambda function




googleUrlBuilder = genHostRequestBuilder "https://google.com"

-- GHCi> googleUrlBuilder "1337hAsk3ll" "book" "1234"
-- "http://google.com/book/1234?token=1337hAsk3ll"



-- It’s clear you run into the same problem again when you look at apiKey. 
-- Passing your API key in each time you call exampleUrlBuilder is still tedious because you’ll likely be using only one or two API keys


genApiRequestBuilder hostBuilder apiKey = (\resource id -> 
                                           hostBuilder apiKey resource id)

myGoogleUrlBuilder = genApiRequestBuilder googleUrlBuilder "1337hAsk3ll"


-- GHCi> myGoogleUrlBuilder "book" "1234"
-- "http://google.com/book/1234?token=1337hAsk3ll"