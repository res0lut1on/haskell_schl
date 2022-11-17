module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  putStrLn "Hello world"
  n <- getLine
  putStrLn ""



myConcat list1 list2 = myConcat' list1 list2 []
    where
        myConcat' (y:ys) (x:xs) outList = myConcat' ys xs (x:y:outList)

myReverse list = myReverse' list []
  where
    myReverse' (x : xs) reserved = myReverse' xs (x:reserved)
    myReverse' [] reserved = reserved

myFilter p (x : xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs
myFilter p [] = []

mySum xs = mySum' 0 xs

mySum' l xs = if null xs then l else mySum' (l + head xs) (tail xs)

mySumTwo xs = mySumTwo' 0 xs
  where
    mySumTwo' l (x : xs) = mySumTwo' (l + x) xs
    mySumTwo' l [] = l -- [] or _ ?

myMap f (x : xs) = f x : myMap f xs
myMap f [] = []

myFor (x : xs) f = f x : myFor xs f
myFor [] f = []

count xs = _count 0 xs

_count l xs = if null xs then l else _count (l + 1) (tail xs)

