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
map f [] = []

count :: Num t => [a] -> t
count xs = count' 0 xs

count' :: Num t => t -> [a] -> t
count' l (x : xs) = if null xs then (l + 1) else count' (l + 1) xs
count' _ [] = 0

insert :: (Ord t, Num t) => a -> t -> [a] -> [a]
insert elem p xs = insert' elem p xs []

insert' :: (Ord t, Num t) => a -> t -> [a] -> [a] -> [a]
insert' elem p (x : xs) list
  | p >= (count xs) + 1 = atLast elem xs -- Если индекс вставки за пределами списка, то вставляю в конец
  | p == 0 = insert' elem (-1) (x : xs) (atLast elem list)
  | otherwise = insert' elem (p - 1) xs (atLast x list)
  where
    atLast a (y : ys) = y : atLast a ys
    atLast a [] = [a]
insert' elem p [] list = list

for :: [t] -> (t -> a) -> [a]
for (x : xs) f = f x : for xs f
for [] f = []

sum :: Num a => [a] -> a
sum xs = sum' 0 xs

sum' :: Num a => a -> [a] -> a
sum' l (x : xs) = if null xs then l + x else sum' (l + x) xs
sum' l [] = l

mySumTwo :: Num t => [t] -> t
mySumTwo xs = mySumTwo' 0 xs
  where
    mySumTwo' l (x : xs) = mySumTwo' (l + x) xs
    mySumTwo' l [] = l -- [] or _ ?

reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' (x : xs) reserved = reverse' xs (x : reserved)
    reverse' [] reserved = reserved

filter :: (a -> Bool) -> [a] -> [a]
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
filter p [] = []

all :: (t -> Bool) -> [t] -> Bool
all p (x : xs) = p x && all p xs
all p [] = True

take :: (Eq t, Num t) => t -> [a] -> [a]
take n (x : xs) = x : take (n - 1) xs
take 0 _ = []
take _ [] = []

skip :: (Eq t, Num t) => t -> [a] -> [a]
skip l (x : xs)
  | l == 0 = x : xs
  | otherwise = skip (l - 1) xs

add :: a -> [a] -> [a]
add a (x : xs) = x : add a xs
add a [] = [a]

removeAt :: (Eq t, Num t) => t -> [a] -> [a]
removeAt i xs = removeAt' i xs []

removeAt' i (x : xs) list = if i == 0 then removeAt' (-1) xs list else x : removeAt' (i - 1) xs list
removeAt' i [] list = list

remove :: Eq a => a -> [a] -> [a]
remove elem xs = remove' elem xs []

remove' elem (x : xs) list
  | x == elem = remove' elem xs list
  | otherwise = x : remove' elem xs list
remove' elem [] list = list

any :: (t -> Bool) -> [t] -> Bool
any p (x : xs) = p x || any p xs
any p [] = False

concat :: [[a]] -> [a]
concat list = concat' list []

concat' (x : xs) list = concat' xs (concat'' x list)
concat' [] list = reverse list -- без реверса не знаю как сделать, иначе получаются элементы [[1,2,3], [4,5,6]] -> [4,5,6,1,2,3]

concat'' (x : xs) list = concat'' xs (x : list)
concat'' [] list = list

while :: (a -> Bool) -> [a] -> [a]
while p xs = while' p xs []

while' p (x : xs) list =
  if p x
    then x : while' p xs (list)
    else list -- list == [] ??

reduce :: Num t1 => (t1 -> t2 -> t1) -> [t2] -> t1
reduce f (x : xs) = reduce' f 0 (x : xs)

reduce' f a (x : xs) = if null xs then (a `f` x) else reduce' f (a `f` x) xs

until :: (t -> Bool) -> (t -> t) -> t -> t
until p f x
  | p x = x
  | otherwise = until p f (f x)

someFunc :: IO ()
someFunc = do
  putStrLn "Hello world"
  n <- getLine
  putStrLn ""

-- take l xs = take' l xs []

-- take' l (x : xs) list
--   | l == 0 = (list)
--   | otherwise = x : take' (l - 1) xs (list)