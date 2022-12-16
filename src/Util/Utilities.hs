{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Utilities
  ( maybeHead,
    unwrap,
    isValidArr,
    toMaybeM,
  )
where

import Control.Monad.Error
import Data.Maybe
import Data.RepEntity.BaseEntity (BaseEntity (entityName), EntityName, returnNameEntity)
import Startup
import Startup ()

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

unwrap :: IO (Maybe (IO (Maybe a))) -> IO (Maybe a)
unwrap mio = mio >>= fromMaybe (return Nothing)

toMaybeM :: (Monad m) => m a -> m (Maybe a)
toMaybeM value = value >>= \val -> return $ Just val

isValidArr :: forall a. (BaseEntity a) => String -> Int -> [a] -> App a
isValidArr methodType eId [] = throwError $ TypeException eId ("Error with Type Method [" ++ methodType ++ "] with Entity = " ++ returnNameEntity (entityName :: EntityName a))
isValidArr _ _ (x : _) = return x