module Data.CommonData
  ( Color (..),
  )
where

data Color = Black | White | Other
  deriving (Show, Read)