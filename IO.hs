 main = print(flip (>) 3 5)

-- putStr :: String -> IO ()
-- putStr xs = sequence_ [putChar x | x <- xs]

-- putBoard = putBoard' 1
-- putBoard' r [] = return ()
-- putBoard' r (n:ns) = do putRow r n 
--                         putBoard' (r + 1) ns

-- putBoard'' xs = sequence_ [putRow r x | x <- xs]