{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Utilities
  ( maybeHead,
    unwrap,
    isValidArr,
    isValid,
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

isValidArr :: String -> [a] -> App a
isValidArr mess [] = throwError $ TypeException mess
isValidArr _ (x : _) = return x

isValid :: String -> Maybe a -> App a
isValid mess Nothing = throwError $ TypeException mess
isValid _ (Just a) = return a

--------------------------------------------------------------------  Почему это не работает?
-- isValidArr :: (BaseEntity a) => String -> Int -> [a] -> App a
-- isValidArr mess eId [] = throwError $ TypeException eId (returnNameEntity (entityName :: EntityName a))
-- isValidArr _ _ (x : _) = return x