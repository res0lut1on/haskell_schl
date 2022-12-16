module Util.Utilities
  ( maybeHead,
    unwrap,
    isValidArr,
    isValid,
  )
where

import Data.Maybe
import Startup
import Control.Monad.Error
import Startup()

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

unwrap :: IO (Maybe (IO (Maybe a))) -> IO (Maybe a)
unwrap mio = mio >>= fromMaybe (return Nothing)

isValidArr :: [a] -> App a
isValidArr [] = throwError NotFound
isValidArr (x:_) = return x

isValid :: Maybe a -> App a
isValid Nothing = throwError NotFound
isValid (Just a) = return a 