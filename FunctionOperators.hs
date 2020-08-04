-- (.) - Composition
-- ($) - Application

stringLength :: Int -> Int
stringLength = length . show

-- >> stringLength 120
-- >> 3

stringLength' :: Show a => a -> Int
stringLength' x = length (show x)

-- f $ x = f x
-- f $ g x = f (g x)
