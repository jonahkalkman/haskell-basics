-- propFilterNoLonger :: (a -> Bool) -> [a] -> Bool
-- propFilterNoLonger p l = length (filter p l) <= length l

-- propFilterAllSatisfy :: (a -> Bool) -> [a] -> Bool
-- propFilterAllSatisfy p l = all p (filter p l)

-- propFilterAllElements :: (a -> Bool) -> [a] -> Bool
-- propFilterAllElements p l = all (`elem` xs) $ filter p xs

-- fac 0 = 1
-- fac l = l * fac l-2

-- propPermsLength xs = length (permutations xs) == fac (length xs)

-- numberOutputs l = permutations l == length l * length l


-- isPerm :: Eq a => [a] -> [a] -> Bool
-- isPerm xs ys = elem ys permutations xs
module Main where 
import Data.List 
  
intersperse' _ []       = []
intersperse' _ [x]      = [x]
intersperse' e (x:y:ys) = x : e : intersperse' e (y:ys)

annotateList :: (t -> a) -> [t] -> [(a,t)]
annotateList _ [] = []
annotateList f (x:xs) = (f x, x) : annotateList f xs

annotate :: Functor f => (t -> a) -> f t -> f(a,t)
annotate f m = fmap (\x -> (f x, x)) m

testing w = do z <- [1,2]
               v <- ['a', 'b']
               return (z,v)

test xs = [(x,y,z) | x <- xs, y <- xs, z <- xs, x + y == z]

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zip'' xs ys = [(x,y) | x <- xs, y <- ys]

main = print()