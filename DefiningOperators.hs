(.+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a,b) .+ (c,d) = (a+c,b+d)
-- We will use .+ because the + operator is already used