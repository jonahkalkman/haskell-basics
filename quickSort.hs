main = print (qsort [3,5,1,4,2], qsortReverse [3,5,1,4,2], qsortFilterDoubles [2,2,3,1,1])

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

qsortFilterDoubles [] = []
qsortFilterDoubles (x:xs) = qsortFilterDoubles smaller ++ [x] ++ qsortFilterDoubles larger
                              where
                                smaller = [a | a <- xs, a < x]
                                larger = [b | b <- xs, b > x]

