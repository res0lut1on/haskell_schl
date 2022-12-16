{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Char (isUpper)
import GHC.Unicode (isLower)

half x =
  if even x
    then Just $ x * 2
    else Nothing

mnd :: Monad m => m a -> m b -> m (a, b)
mnd xs ys = do
  y <- ys
  x <- xs
  return (x, y)

blockyPlain xs ys =
  xs
    >>= \x ->
      ys
        >>= \y -> return (x, y)

type SimpleState s a = s -> (a, s)

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

applyMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f (Just x) = f x

data Box a = Box a deriving (Show)

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f (Box a) = Box (f a)

-- <$> :: Functor f ::          (a -> b) -> f a -> f b
-- <*> :: Applicative f =>      f (a -> b) -> f a -> f b
-- >>= :: Monad m =>            m a -> (a -> m b) -> m b
-- pure :: Applicative f => a -> f a

--  Если ваше выражение включает в себя 3 вычисления, которые могут завершиться неудачно,
--   сбой любого из них приведет к сбою всего вычисления. Интерфейс Monadстал более гибким.
--   Это позволяет потоку управления «эффектом» зависеть от значений «результата».

--  всегда выполняют эффекты всех своих аргументов

-- Вы не можете сделать это с Applicatives - значение результата одного эффективного вычисления не может определить, какой эффект последует.
-- С монадами последующие эффекты могут зависеть от предыдущих значений. Например, у вас может быть:

-- bind: я дам вам содержащееся значение, а вы вернете мне новое упакованное значение.
-- даете мне упакованную функцию, которая принимает содержащееся значение и возвращает значение, и я буду использовать ее для создания нового упакованного значения на основе моих правил.

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c x y = c >>= \z -> if z then x else y

ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA c x y = (\c' x' y' -> if c' then x' else y') <$> c <*> x <*> y

whileM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
whileM p step x = ifM (p x) (step x >>= whileM p step) (return x)

-- whileA :: Applicative f => (a -> f Bool) -> (a -> f a) -> a -> f a
-- whileA p step x = ifA (p x) (whileA p step <*> step x) (pure x)


maxPairs :: (Monad m, Ord a) => m (a, a) -> m a
maxPairs val = val >>= (\kort -> return $ uncurry max kort)

-- maybe (return Nothing) (runMaybeT . f)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

newtype MaybeIOT a = MaybeIOT
  { runMaybeIOT :: IO (Maybe a)
  }

