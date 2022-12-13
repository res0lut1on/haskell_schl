module Util.Utilities
  ( maybeHead,
    unwrap,
  )
where

import Data.Maybe

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

unwrap :: IO (Maybe (IO (Maybe a))) -> IO (Maybe a)
unwrap mio = mio >>= fromMaybe (return Nothing)