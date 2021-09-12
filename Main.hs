-- main = print (map thrice (sums [0..2]))

-- main = interact (intercalate " / " . map reverse.lines)
  -- where work text = intercalate " / " (map reverse (lines text))

-- thrice x = [x, x, x]

-- sums (x : y : ys) = x : sums (x + y : ys)
-- sums xs           = xs

main = print (pow2 3)®

-- pow       :: Int -> Int -> Int
-- x `pow` 0 = 1
-- x `pow` n | even n    = let y = x `pow` (n `div` 2) in y * y
--           | otherwise = x  * (x `pow` (n-1))

pow2   :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)
