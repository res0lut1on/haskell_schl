{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
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
    concat,
    reduce,
    until,
    groupBy,
    flip,
    add,
    take,
    foldTest,
    whileDo,
  )
where

import Prelude hiding (add, all, any, concat, elem, filter, flip, foldl, foldl', foldr, for, groupBy, init, map, max, min, reduce, reverse, seq, sum, take, until)

max :: Ord a => [a] -> a
max (x : xs) = foldl (\acc curr -> if curr > acc then curr else acc) x xs

concat :: [[a]] -> [a]
concat = reverse . foldl (\acc curr -> append curr acc) []
  where
    append [] xs = xs
    append (e : es) xs = append es (e : xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x : xs) = foldl f (f init x) xs

--      [1]   2  [3,4]             [1]  2  [3,4]
--      [2,1] 3  [4]               [2,1] 3  [4]
--    [3,2,1] 4  [ ]            [3,2,1] 4  []
--  [4,3,2,1] [] [ ]    ------[4,3,2,1]------

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para c n (x : xs) = c x xs (para c n xs)
--    + 1  2  [3,4]  + 2 [3,4]    + 1 [3,4]
--    + 1  3  [4]    + 3 [4]      + 1 [4]
--    + 1  4  []     + 4 []      ---1---
para _ n [] = n

foldrp c = para (\curr _ acc -> c curr acc)

--     + 1 [2,3,4]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr f init (x : xs) = f x (foldr f init xs)

--      [1]   2  [3,4]    2          [1] [3,4]
--      [1]   3  [4]      3          [1] [4]
--      [1]   4  []      4           [1] []
--      [1]      []          ------[1]------

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x : xs) =
  let z' = f z x
   in z' `seq` foldl' f z' xs

reduce :: (t -> t -> t) -> [t] -> t
reduce f (x : xs) = foldr (\acc curr -> f curr acc) x xs

while :: (a -> Bool) -> [a] -> [a]
while p = foldr (\curr acc -> if p curr then curr : acc else []) []

whileDo :: (a -> Bool) -> [a] -> [a]
whileDo p = foldr (\curr acc -> if p curr then curr : acc else [curr]) []

seq :: a -> b -> b
seq _ b = b

add :: a -> [a] -> [a]
add elem xs = elem : foldr (:) [] xs

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

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f v = until' p f v (unt p f v)
  where
    until' p _ = foldr (\x acc -> if p x then x else acc)
    unt p f v
      | p v = []
      | otherwise = f v : unt p f (f v)

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

removeAt :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
removeAt 0 (_ : xs) = xs
removeAt l xs = foldr g z xs 0
  where
    g x r i
      | i == l = r (i + 1)
      | otherwise = x : r (i + 1)
    z _ = []

remove :: Eq a => a -> [a] -> [a]
remove elem xs = foldr g z xs elem
  where
    g x r i
      | i == x = r i
      | otherwise = x : r i
    z _ = []

insert :: (Eq a1, Num a1) => a2 -> a1 -> [a2] -> [a2]
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

-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy cond = post . foldr fn ([], [])
--   where
--     post ([], yss) = yss
--     post (xs, yss) = xs : yss

--     fn a ([], yss) = ([a], yss)
--     fn a (b : bs, yss)
--       | cond a b = (a : b : bs, yss)
--       | otherwise = ([a], (b : bs) : yss)

data Product = Product
  { prodId :: Int,
    name :: String,
    price :: Double
  }

instance Eq Product where
  x == y = price x == price y

instance Show Product where
  show (Product _ n pr) = n ++ " = " ++ show pr

ord :: [Product]
ord =
  [ Product {prodId = 1, price = 1, name = "Korona Extra"},
    Product {prodId = 2, price = 1, name = "Biver"},
    Product {prodId = 4, price = 10, name = "Water"},
    Product {prodId = 8, price = 7, name = "Latte"},
    Product {prodId = 6, price = 5, name = "Apple"},
    Product {prodId = 7, price = 7, name = "Juice"},
    Product {prodId = 5, price = 10, name = "Ice Cream"},
    Product {prodId = 3, price = 7, name = "Hennesy"}
  ]

-- (price, Product)
groupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy f xs = foldr (\curr acc -> snd curr : acc) [] $ foldr step [] xs
  where
    step curr acc
      | haveKeyInGroup (f curr) acc = addInGroup curr acc
      | otherwise = (f curr, [curr]) : acc

    haveKeyInGroup :: Eq a => a -> [(a, b)] -> Bool
    haveKeyInGroup key = foldr (\x xs -> fst x == key || xs) False

    addInGroup curr =
      foldr
        ( \xs xss ->
            if fst xs == f curr
              then (f curr, curr : snd xs) : xss
              else xs : xss
        )
        []

-- [(10, IceCream),(7,Hennesy, Juice)]

foldTest :: IO ()
foldTest = do
  putStrLn ""
  putStr "foldl (/) 27 [3,1,3] = "
  print (foldl (/) 27 [3, 1, 3])
  putStrLn ""
  putStr "foldr (*) 1 [3,3,3] = "
  print (foldr (*) 1 [3, 3, 3])
  putStrLn ""
  putStr "foldl' (+) 0 [1,2,3] = "
  print (foldl' (+) 0 [1, 2, 3])
  putStrLn ""
  putStr "map (*2) [4,3,2,1,5] = "
  print (show (map (* 2) [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "for [4,3,2,1,5] (*3) = "
  print (show (for [4, 3, 2, 1, 5] (* 3)))
  putStrLn ""
  putStr "filter even [4,3,2,1,5] = "
  print (show (filter even [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "all odd [4,3,2,1,5] = "
  print (show (all odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "count [1,2,3,4,5]) = "
  print (count [1, 2, 3, 4, 5])
  putStrLn ""
  putStr "any odd [4,3,2,1,5] = "
  print (show (any odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "max [4,7,2,1,5] = "
  print (show (max [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "min [4,7,2,1,5] = "
  print (show (min [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "sum [4,7,2,1,5] = "
  print (show (sum [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "reverse [4,7,2,1,5] = "
  print (show (reverse [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "take 3 [4,7,2,1,5] = "
  print (show (take 3 [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "skip 3 [4,7,2,1,5] = "
  print (show (skip 3 [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "add 3 [4,7,2,1,5] = "
  print (show (add 3 [4, 7, 2, 1, 5]))
  putStrLn ""
  putStr "while odd [1,1,3,4,5,5,6,7,8,9] = "
  print (while odd [1, 1, 3, 4, 5, 5, 6, 7, 8, 9])
  putStrLn ""
  putStr "removeAt 1 [4,3,2,1,5] = "
  print (removeAt 1 [4, 3, 2, 1, 5])
  putStrLn ""
  putStr "remove 2 [4,3,2,1,5] = "
  print (show (remove 2 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "insert 11 2 [4,3,2,1,5] = "
  print (show (insert 11 2 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "until (>100) (*2) 1 = "
  print (show (until (> 100) (* 2) 1))
  putStrLn ""
  putStr "whileDo odd [1,1,3,4,5,5,6,7,8,9] = "
  print (show (whileDo odd [1, 1, 3, 4, 5, 5, 6, 7, 8, 9]))
  putStrLn ""
  putStr "concat [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13]] = "
  print (show (concat [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13]]))
  putStrLn ""
  putStrLn "groupBy price [Korona Extra, Biver, Water, Ice Cream, Apple, Juice, Latte, Hennesy] = "
  putStrLn ""
  print (show (groupBy price ord))
  putStrLn ""

-- putStr "groupBy (x y -> (x * y `mod` 3) [1, 2, 3, 4, 5, 6, 7, 8, 9] = "
-- print (show (groupBy (==) ord))
-- putStrLn ""
