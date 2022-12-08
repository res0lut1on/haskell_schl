module Services.ApplyFilter (applyFilter, pagination) where

import Data.SearchModel

applyFilter :: (SearchModel c) => (a -> b) -> (c -> Maybe b) -> (b -> b -> Bool) -> c -> [a] -> [a]
applyFilter valueSearchEntityField valueSearchModelField filt searchModel entityList =
  let maybeValueSearchModelField = valueSearchModelField searchModel
   in ff maybeValueSearchModelField
  where
    ff (Just value) = filter (\ent -> filt (valueSearchEntityField ent) value) entityList
    ff Nothing = entityList

pagination :: Int -> Int -> [a] -> [a]
pagination page sizePage = take sizePage . drop ((page - 1)* sizePage) 