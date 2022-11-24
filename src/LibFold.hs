module LibFold
  ( module Prelude,
    foldl,
    foldr,
    foldl',
    foldrp,
    para,
    seq,
    map,
    count,
    for,
    insert,
    sum,
    filter,
    reverse,
    skip,
    min,
    max,
    all,
    any,
    remove,
    removeAt,
    while,
    -- concat,
    reduce,
    -- until,
    -- groupBy,
    flip,
    add,
    take,
    foldTest,
  )
where

import Prelude hiding (add, all, any, flip, concat, elem, filter, foldl, foldl', foldr, for, init, map, max, min, reduce, reverse, seq, sum, take, until)

max :: Ord a => [a] -> a
max (x : xs) = foldl (\acc curr -> if curr > acc then curr else acc) x xs

-- concat (x : xs) = concat' x xs 
--   where concat' currs (x:xs) = 

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x : xs) = foldl f (f init x) xs
--      [1]   2  [3,4]             [1]  2  [3,4]
--      [2,1] 3  [4]               [2,1] 3  [4]
--    [3,2,1] 4  [ ]            [3,2,1] 4  []
--  [4,3,2,1] [] [ ]    ------[4,3,2,1]------

para  :: (a -> [a] -> b -> b) -> b -> [a] -> b
para  c n (x : xs) = c x xs (para c n xs)
--    + 1  2  [3,4]  + 2 [3,4]    + 1 [3,4]
--    + 1  3  [4]    + 3 [4]      + 1 [4]
--    + 1  4  []     + 4 []      ---1---
para  _ n [] = n

foldrp c n xs = para  (\curr xs acc -> c curr acc) n xs
--     + 1 [2,3,4]                  

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr f init (x : xs) = f x (foldr f init xs)
--      [1]   2  [3,4]    2          [1] [3,4]
--      [1]   3  [4]      3          [1] [4]
--      [1]   4  []      4           [1] []
--      [1]      []          ------[1]------

reduce f (x:xs) = foldr (\acc curr -> f curr acc) x xs

-- until p f x
--   | p x = x
--   | otherwise = until p f (f x)

until p f v = until' p f v [1..10]
  where until' p f v xs = foldl (\acc curr -> if p acc then acc else f acc) v xs
--        (>100)(*2)1 []    

while :: (a -> Bool) -> [a] -> [a]
while p = foldr (\curr acc -> if p curr then curr:acc else []) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x : xs) =
  let z' = f z x
   in z' `seq` foldl' f z' xs

seq :: a -> b -> b
seq _ b = b

add elem xs = elem:foldr (:) [] xs
--                   [1]   2                                                     2     [1]
--                   [2,1] 3                                                      3   [2,1]
--                [3,2,1]  4                                                      4  [3,2,1]
--              [4,3,2,1]  n

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

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

removeAt 0 (_:xs) = xs
removeAt l xs = foldr g z xs 0
  where
    g x r i
      | i == l = r (i + 1)
      | otherwise = x : r (i + 1)
    z _ = []


remove elem xs = foldr g z xs elem
  where
    g x r i
      | i == x = r i
      | otherwise = x : r i
    z _ = []

insert elem pos xs = foldr g z xs pos
  where
    g x r i
      | i == 0 = elem : x : r (i - 1)
      | otherwise = x : r (i - 1)
    z _ = []

skip :: Int -> [a] -> [a]
skip 0 xs = xs
skip n xs = foldr g z xs 0 xs
  where
    g _ r i xs@(_ : t)
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
  putStrLn ""
  putStrLn "add 3 [4,7,2,1,5] = "
  putStrLn ""
  print (show (add 3 [4, 7, 2, 1, 5]))
  putStrLn ""
  putStrLn "while odd [1,1,3,4,5,5,6,7,8,9] = "
  putStrLn ""
  print (while odd [1,1,3,4,5,5,6,7,8,9])
  putStrLn ""
  putStrLn "removeAt 1 [4,3,2,1,5] = "
  print (removeAt 1 [4, 3, 2, 1, 5])
  putStrLn ""
  putStr "remove 2 [4,3,2,1,5] = "
  print (show (remove 2 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStrLn "insert 11 2 [4,3,2,1,5] = "
  print (show (insert 11 2 [4, 3, 2, 1, 5]))
  putStrLn ""
  
  -- putStrLn ""
  -- putStr "concat [[1,1,3,4,5], [4,3,2,1,5], [4,1,5]] = "
  -- print (show (concat [[1, 1, 3, 4, 5], [4, 3, 2, 1, 5], [4, 1, 5]]))
  -- putStrLn ""
