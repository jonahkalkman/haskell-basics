main = print((2134835688 / 31557600) * 0.2408467)

do x <- [1,2,3]
   y <- [1,2,3]
   True <- return (x /= y)
   return (x,y)
  
[1,2,3] >>= (\ x -> [1,2,3] >>= (\ y -> return (x /= y) >>= (\r  -> if r == True then return (x,y) else fail "")))


do p <- e1; e2 
e1 >>= \ p -> e2

sequence_ = do x <- a
               xs <- sequence_ acts
               return (x:xs)

sequence_ = a >>= \x -> sequence_ acts >>= \xs -> return(x:xs)


-- Functor instances

-- Given the type:
data Error m a = Error m | OK a

instance Functor (Error m) where
  fmap (Error m) = Error m
  fmap (OK a) = OK (f a)

instance Monad (Error m) where
  return = OK
  (OK a) >>= f = f a
  (Error m) >>= _ = Error m
