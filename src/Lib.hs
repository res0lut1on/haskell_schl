module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  putStrLn "Hello world"
  n <- getLine
  putStrLn ""
