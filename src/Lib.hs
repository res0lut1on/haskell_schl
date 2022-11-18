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

count :: Num t => [a] -> t
count xs = count' 0 xs
  where
    count' l (_ : xs) = count' (l + 1) xs
    count' l [] = l

insert :: (Ord t, Num t) => a -> t -> [a] -> [a]
insert elem p xs = insert' elem p xs []
  where
    insert' elem p (x : xs) list
      | p >= (count xs) + 1 = atLast elem xs -- Если индекс вставки за пределами списка, то вставляю в конец
      | p == 0 = insert' elem (-1) (x : xs) (atLast elem list)
      | otherwise = insert' elem (p - 1) xs (atLast x list)
      where
        atLast a (y : ys) = y : atLast a ys
        atLast a [] = [a]
    insert' _ _ [] list = list

for :: [t] -> (t -> a) -> [a]
for (x : xs) f = f x : for xs f
for [] _ = []

sum :: Num a => [a] -> a
sum xs = sum' 0 xs
  where
    sum' :: Num a => a -> [a] -> a
    sum' l [x] = l + x
    sum' l (x : xs) = sum' (l + x) xs
    sum' l [] = l

reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' (x : xs) reserved = reverse' xs (x : reserved)
    reverse' [] reserved = reserved

filter :: (a -> Bool) -> [a] -> [a]
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
filter _ [] = []

all :: (t -> Bool) -> [t] -> Bool
all p (x : xs) = p x && all p xs
all _ [] = True

take l xs = take' l xs []
  where
    take' l (x : xs) list
      | l == 0 = (list)
      | otherwise = x : take' (l - 1) xs (list)

skip :: (Eq t, Num t) => t -> [a] -> [a]
skip l (x : xs)
  | l == 0 = x : xs
  | otherwise = skip (l - 1) xs

add :: a -> [a] -> [a]
add a (x : xs) = x : add a xs
add a [] = [a]

removeAt :: (Eq t, Num t) => t -> [a] -> [a]
removeAt i xs = removeAt' i xs []
  where
    removeAt' i (x : xs) list
      | i == 0 = removeAt' (-1) xs list
      | null xs = list
      | otherwise = x : removeAt' (i - 1) xs list

remove :: Eq a => a -> [a] -> [a]
remove elem xs = remove' elem xs []
  where
    remove' elem (x : xs) list
      | x == elem = remove' elem xs list
      | otherwise = x : remove' elem xs list
    remove' _ [] list = list

any :: (t -> Bool) -> [t] -> Bool
any p (x : xs) = p x || any p xs
any _ [] = False

concat :: [[a]] -> [a]
concat list = concat' list []
  where
    concat' (x : xs) list = concat' xs (concat'' x list)
    concat' [] list = reverse list
    concat'' (x : xs) list = concat'' xs (x : list)
    concat'' [] list = list

while :: (a -> Bool) -> [a] -> [a]
while p xs = while' p xs []
  where
    while' p (x : xs) list
      | p x = x : while' p xs (list)
      | otherwise = list

reduce :: Num t => (t -> a -> t) -> [a] -> t
reduce f (x : xs) = reduce' f 0 xs
  where
    reduce' f a (x : xs) = reduce' f (a `f` x) xs
    reduce' f a [] = (a `f` x)

until :: (t -> Bool) -> (t -> t) -> t -> t
until p f x
  | p x = x
  | otherwise = until p f (f x)

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
  print (reduce (-) [1, 2, 3, 4, 5])
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
