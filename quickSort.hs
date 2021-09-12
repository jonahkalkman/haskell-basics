main = print (qsort [3,5,1,4,2], qsortReverse [3,5,1,4,2])

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]

qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
                        where
                          smaller = [a | a <- xs, a <= x]
                          larger = [b | b <- xs, b > x]

