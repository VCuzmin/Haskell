-- A list is either an empty list or an element followed by another list.

-- When taking apart a list, the main pieces are the head, the tail, and the end (represented by []). The head is just the first element in a list:

GHCi> head [1,2,3]
1
GHCi> head [[1,2],[3,4],[5,6]]
[1,2]

-- The tail is the rest of the list left over, after the head:

GHCi> tail [1,2,3]
[2,3]
GHCi> tail [3]
[]


-- To build a list, you need just one function and the infix operator (:), which is called CONS. 
-- This term is short for construct and has its origins in Lisp. We’ll refer to this operation as consing, because : looks a bit odd in a sentence.

GHCi> 1:[]
[1]

GHCi> 1:2:3:4:[]
[1,2,3,4]

GHCi> (1,2):(3,4):(5,6):[]
[(1,2),(3,4),(5,6)]

GHCi> 1:[2,3,4]
[1,2,3,4]

GHCi>['h','e','l','l','o']
"hello"
GHCi> 'h':'e':'l':'l':'o':[]
"hello"

-- An important thing to remember is that in Haskell every element of the list must be the same type. For example, you can cons the letter 'h' to the string "ello" because "ello" is just a list of characters and 'h' (single quotes) is a character:

GHCi> 'h':"ello"
"hello"

-- If you do want to combine two lists, you need to concatenate them by using ++. 
-- You saw this in lesson 3 with concatenating text, but given that strings are just lists, it will work on any list:

GHCi> "h" ++ "ello"
"hello"
GHCi> [1] ++ [2,3,4]
[1,2,3,4]