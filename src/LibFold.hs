module LibFold
  ( module Prelude,
    foldl,
    foldr,
    foldl',
    seq,
    map,
    count,
    for,
    -- insert,
    sum,
    filter,
    reverse,
    skip,
    min,
    max,
    all,
    any,
    -- remove,
    -- removeAt,
    -- while,
    -- concat,
    reduce,
    -- until,
    -- someFunc,
    -- add,
    take,
    foldTest,
  )
where

import Prelude hiding (add, all, any, concat, filter, foldl, foldl', foldr, for, init, map, max, min, reduce, reverse, seq, sum, take, until)

max :: Ord a => [a] -> a
max (x : xs) = foldl (\acc curr -> if curr > acc then curr else acc) x xs

-- concat (x : xs) = foldr (\curr (ys) -> foldl : curr ys) x xs

--                       []   a:[a]
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x : xs) = foldl f (f init x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr f init (x : xs) = f x (foldr f init xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x : xs) =
  let z' = f z x
   in z' `seq` foldl' f z' xs

seq :: a -> b -> b
seq _ b = b

map :: (t -> a) -> [t] -> [a]
map f = foldr (\y ys -> f y : ys) []

for :: [t] -> (t -> a) -> [a]
for xs f = foldr (\y ys -> f y : ys) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\y ys -> if f y then y : ys else ys) []

all :: (a -> Bool) -> [a] -> Bool
all f = foldl (\acc curr -> f curr && acc) True

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\curr acc -> f curr || acc) False

sum :: Num b => [b] -> b
sum (x : xs) = foldl' (+) x xs

count :: [a] -> Integer
count = foldr (\_ j -> 1 + j) 0

min :: Ord a => [a] -> a
min (x : xs) = foldl (\acc curr -> if curr < acc then curr else acc) x xs

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce = foldl

reverse :: [a] -> [a]
reverse = foldl (\acc curr -> curr : acc) []

take :: (Ord a1, Num a1) => a1 -> [a2] -> [a2]
take 0 _ = []
take l xs = foldr g z xs 0
  where
    g x r i
      | i >= l = []
      | otherwise = x : r (i + 1)
    z _ = []

skip :: Int -> [a] -> [a]
skip 0 xs = xs
skip n xs = foldr g z xs 0 xs
  where
    g x r i xs@(_ : t)
      | i >= n = xs
      | otherwise = r (i + 1) t
    z _ _ = []



foldTest :: IO ()
foldTest = do
  putStrLn ""
  putStrLn "foldl (/) 27 [3,1,3] = "
  putStrLn ""
  print (foldl (/) 27 [3, 1, 3])
  putStrLn ""
  putStrLn "foldr (*) 1 [3,3,3] = "
  putStrLn ""
  print (foldr (*) 1 [3, 3, 3])
  putStrLn ""
  putStrLn "foldl' (+) 0 [1,2,3] = "
  putStrLn ""
  print (foldl' (+) 0 [1, 2, 3])
  putStrLn ""
  putStrLn "map (*2) [4,3,2,1,5] = "
  putStrLn ""
  print (show (map (* 2) [4, 3, 2, 1, 5]))
  putStrLn ""
  putStrLn "for [4,3,2,1,5] (*3) = "
  print (show (for [4, 3, 2, 1, 5] (* 3)))
  putStrLn ""
  putStrLn "filter even [4,3,2,1,5] = "
  putStrLn ""
  print (show (filter even [4, 3, 2, 1, 5]))
  putStrLn ""
  putStrLn "all odd [4,3,2,1,5] = "
  putStrLn ""
  print (show (all odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStrLn "count [1,2,3,4,5]) = "
  putStrLn ""
  print (count [1, 2, 3, 4, 5])
  putStrLn ""
  putStrLn "any odd [4,3,2,1,5] = "
  putStrLn ""
  print (show (any odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStrLn "max [4,7,2,1,5] = "
  putStrLn ""
  print (show (max [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "min [4,7,2,1,5] = "
  putStrLn ""
  print (show (min [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "sum [4,7,2,1,5] = "
  putStrLn ""
  print (show (sum [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "reverse [4,7,2,1,5] = "
  putStrLn ""
  print (show (reverse [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "take 3 [4,7,2,1,5] = "
  putStrLn ""
  print (show (take 3 [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "skip 3 [4,7,2,1,5] = "
  putStrLn ""
  print (show (skip 3 [4, 7, 2, 1, 5]))

--   putStrLn ""
--   putStr "concat [[1,1,3,4,5], [4,3,2,1,5], [4,1,5]] = "
--   print (show (concat [[1, 1, 3, 4, 5], [4, 3, 2, 1, 5], [4, 1, 5]]))
--   putStrLn ""
