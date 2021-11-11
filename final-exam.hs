-- Question 1
-- dequeToList mySmallDeque == [1,2,3,4,5]
-- dequeToList myDeque == [1,2,3,4,5,6,7,8,9,10,11]
-- Hint: you may want to write a helper function 'flatten :: [(a,a)] -> [a]' first.

-- data Deque a = Empty
-- | Single a
-- | Multiple (Access a) (Deque (a,a)) (Access a)
-- deriving (Show,Eq)
-- where
-- data Access a = One a | Two a a deriving (Show,Eq)

-- myDeque :: Deque Int
-- myDeque = Multiple 
                  -- (Two 1 2)
                    -- (Multiple 
                    --    (One (3,4))
                      --  (Single ((5,6),(7,8)))
                      --  (One (9,10)))
                  -- (One 11)

-- mySmallDeque :: Deque Int
-- mySmallDeque = Multiple (One 1)
-- (Single (2,3))
-- (Two 4 5)

toList Empty            = []
toList (Single x)       = [x]
toList (Multiple l m r) = toList' l ++ flatten (toList m) ++ toList' r
  where
    toList' (One x)   = [x]
    toList' (Two x y) = [x,y]
    flatten = concatMap (\(x,y) -> [x,y])

-- b. [4pt] Write a total function 'safeLast' that gets the last (rightmost) element in the Deque (if the
-- Deque is non-empty). For example, we have that
-- safeLast myDeque == Just 11
-- Your implementation should use constant (O(1)) time (so you cannot convert the Deque into a list
-- first).

safeLast :: Deque -> Maybe a 
safeLast Empty = Nothing
safeLast (Single x) = Just x
safeLast (Multiple _ _ r) = isLast r
                              where
                                isLast (One x) = Just x
                                isLast (Two x y) = Just (x,y)

-- c. [4pt] Implement a function 'cons' that takes an 'x :: a' and a 'dq :: Deque a' and adds the 'x' to the
-- front of 'dq' (analogous to how
-- the '(:) :: a -> [a] -> [a]' function/constructor adds an element to the front of a list. For example,
-- cons 1 Empty == Single 1
-- cons 0 mySmallDeque == Multiple (Two 0 1) (Single (2,3)) (Two 4 5)
-- Your implementation should run in O(log n) time. You do not have to prove/argue that your
-- implementation achieves this O(log n) time bound; the most natural implementation will achieve this.
-- Hint: the function 'cons' will be a recursive function
--  Multiple (Access a) (Deque (a,a)) (Access a)
-- Bijna goed, alleen recursive stap gaat fout met cons

cons :: a -> Deque a -> Deque a
cons x Empty = Single x
cons x (Single a) = Multiple (One x) Empty (One a)
cons x (Multiple l m r) = whatIs l 
                          where 
                            whatIs (One a) = Multiple (Two a x) m r
                            whatIs (Two a b) = Multiple (One x) (cons (a,b) m) r

-- d. [4pt] Make the 'Deque' data type an instance of the 'Functor' typeclass. You may assume that
-- 'Access' has already been made an instance of 'Functor'.

instance Functor Deque where 
  fmap f Empty = Empty
  fmap f (Single a) = Single(f a)
  fmap f (Multiple l m r) = Multiple (fmap f l) (fmap (\(x,y) -> (f x, f y)) m) (fmap f r)

 
isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] _ = True
isPrefixOf (x:xs) [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys


-- (6 points) Recall the data type First a as defined in the first exercise. Consider a function findFirst
-- :: [a] -> [a] -> First Int that, given arguments xs and ys returns TheFirst i if and only
-- if xs is a contiguous (in Dutch: ”aaneengesloten”) sublist of ys starting at index i. The function
-- returns Never if and only if xs does not occur as a contiguous sublist of ys.
-- Using findFirst, write a function before :: Eq a => [a] -> [a] -> [a] -> Bool for which
-- before xs ys zs returns True if and only if xs and ys are contiguous sublists of zs and (the first
-- occurrence of) xs starts no later than (the first occurrence of) ys in the list zs.

before :: Eq a => [a] -> [a] -> [a] -> Bool
before xs ys zs = firstList <= secondList && secondList < Never
                  where
                    firstList | findFirst xs zs == TheFirst i = True
                              | findFirst xs zs == Never = False
                    secondList | findFirst ys zs == TheFirst i = True
                               | findFirst ys zs == Never = False
