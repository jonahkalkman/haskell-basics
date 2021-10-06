-- https://wiki.haskell.org/99_questions/1_to_10

-- (*) Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' [] = error "Empty list"
myLast' xs = last xs

myLast'' :: [a] -> a
myLast'' = head . reverse

-- (*) Find the last but one element of a list.

myButLast :: [a] -> a
myButLast xs@(a:b) | length xs > 2 = myButLast b
                   | otherwise = a

myButLast' (x:[y]) = x
myButLast' (x:xs) = myButLast' xs

-- (*) Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _ = error "Index out of bounds"

-- (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' = foldr (\x n -> n + 1) 0

main = print(myLength' [1,2,3,4])