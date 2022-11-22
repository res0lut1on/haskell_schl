module Utilities
  ( maybeHead,
  )
where

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x