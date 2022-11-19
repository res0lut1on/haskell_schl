module Lib
  ( module Prelude,
    map,
    count,
    for,
    insert,
    sum,
    filter,
    reverse,
    skip,
    all,
    any,
    remove,
    removeAt,
    while,
    concat,
    reduce,
    until,
    someFunc,
    take,
  )
where

import Prelude hiding (all, any, concat, filter, map, reverse, sum, take, until)

map :: (t -> a) -> [t] -> [a]
map f (x : xs) = f x : map f xs
map _ [] = []

count :: Num a1 => [a2] -> a1
count (_ : xs) = 1 + count xs
count [] = 0

for :: [t] -> (t -> a) -> [a]
for (x : xs) f = f x : for xs f
for [] _ = []

filter :: (a -> Bool) -> [a] -> [a]
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
filter _ [] = []

all :: (t -> Bool) -> [t] -> Bool
all p (x : xs) = p x && all p xs
all _ [] = True

take :: (Eq t, Num t) => t -> [a] -> [a]
take l (x : xs)
  | l /= 0 = x : take (l - 1) xs
  | otherwise = []
take _ [] = []

skip :: (Eq t, Num t) => t -> [a] -> [a]
skip l (x : xs)
  | l == 0 = x : xs
  | otherwise = skip (l - 1) xs

add :: a -> [a] -> [a]
add a (x : xs) = x : add a xs
add a [] = [a]

removeAt :: (Eq t, Num t) => t -> [a] -> [a]
removeAt i (x : xs)
  | i == 0 = removeAt (-1) xs
  | otherwise = x : removeAt (i - 1) xs
removeAt _ [] = []

remove :: Eq a => a -> [a] -> [a]
remove elem (x : xs)
  | x == elem = remove elem xs
  | otherwise = x : remove elem xs
remove _ [] = []

any :: (t -> Bool) -> [t] -> Bool
any p (x : xs) = p x || any p xs
any _ [] = False

until :: (t -> Bool) -> (t -> t) -> t -> t
until p f x
  | p x = x
  | otherwise = until p f (f x)

while :: (a -> Bool) -> [a] -> [a]
while p (x : xs)
  | p x = x : while p xs
  | otherwise = []

sum :: Num a => [a] -> a
sum (x : xs) = x + sum xs
sum [] = 0

concat :: [[a]] -> [a]
concat list = concat' list []
  where
    concat' (x : xs) list = concat' xs (concat'' x list)
    concat' [] list = reverse list
    concat'' (x : xs) list = concat'' xs (x : list)
    concat'' [] list = list

reduce _ [x] = x
reduce f (x : xs) = f x (reduce f xs)

reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' (x : xs) reserved = reverse' xs (x : reserved)
    reverse' [] reserved = reserved

insert elem p (x : xs)
  | p >= (count xs) = x : insert elem p xs
  | p == 0 = elem : insert elem (-1) (x : xs)
  | otherwise = x : insert elem (p - 1) xs
insert elem 0 [] = [elem]
insert _ _ [] = []

someFunc :: IO ()
someFunc = do
  putStrLn ""
  putStr "sum [1,2,3,4,5]) = "
  print (sum [1, 2, 3, 4, 5])
  putStrLn ""
  putStr "count [1,2,3,4,5]) = "
  print (count [1, 2, 3, 4, 5])
  putStrLn ""
  putStr "until (> 50) (*3) 2 = "
  print (until (> 50) (* 3) 2)
  putStrLn ""
  putStr "reduce (*) [1,2,3,4,5] = "
  print (reduce (*) [1, 2, 3, 4, 5])
  putStrLn ""
  putStr "while odd [1,1,3,4,5] = "
  print (while odd [1, 1, 3, 4, 5])
  putStrLn ""
  putStr "concat [[1,1,3,4,5], [4,3,2,1,5], [4,1,5]] = "
  print (show (concat [[1, 1, 3, 4, 5], [4, 3, 2, 1, 5], [4, 1, 5]]))
  putStrLn ""
  putStr "any odd [4,3,2,1,5] = "
  print (show (any odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "all odd [4,3,2,1,5] = "
  print (show (all odd [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "remove 2 [4,3,2,1,5] = "
  print (show (remove 2 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "removeAt 1 [4,3,2,1,5] = "
  print (show (removeAt 0 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "add 10 [4,3,2,1,5] = "
  print (show (add 10 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "skip 3 [4,3,2,1,5] = "
  print (show (skip 3 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "take 3 [4,3,2,1,5] = "
  print (show (take 3 [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "filter even [4,3,2,1,5] = "
  print (show (filter even [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "reverse [4,3,2,1,5] = "
  print (show (reverse [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "map (*2) [4,3,2,1,5] = "
  print (show (map (* 2) [4, 3, 2, 1, 5]))
  putStrLn ""
  putStr "for [4,3,2,1,5] (*3) = "
  print (show (for [4, 3, 2, 1, 5] (* 3)))
  putStrLn ""
  putStr "insert 11 2 [4,3,2,1,5] = "
  print (show (insert 11 2 [4, 3, 2, 1, 5]))
  putStrLn ""
