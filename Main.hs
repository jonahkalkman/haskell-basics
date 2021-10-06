-- or recursive and with foldr

or' :: [Bool] -> Bool
-- or' []     = False
-- or' (x:xs) = x || or' xs

or' = foldr (||) False

-- concatMap recursive 

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

-- unlines recursive

unlines' :: [String] -> String
unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

-- unzip recursive

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((xt,yt):xs) = (xt : first, yt : second)
                        where (first,second) = unzip' xs

-- merge recursive

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' l@(x : xs) r@(y : ys) | x <= y    = x : merge' xs r
                             | otherwise = y : merge' l ys

-- MAIN 
main = print(or' [True, False])
