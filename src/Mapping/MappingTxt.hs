{-# LANGUAGE InstanceSigs #-}
module Mapping.MappingTxt (MappEntity (..)) where

class MappEntity a where
  mappEntityTo :: a -> String
  mappEntityFrom :: String -> a
  
