class GenericMapping a where
  toModel :: a -> b

  toList :: [a] -> [b]