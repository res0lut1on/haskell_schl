module Data.MappingTxt (MappEntity (..)) where

class MappEntity a where
  mappEntityTo :: a -> String
  mappEntityFrom :: String -> a
