module Services.ApplyFilter (applyFilter, pagination) where

applyFilter :: (a -> b) -> (c -> Maybe b) -> (b -> b -> Bool) -> c -> [a] -> [a]
applyFilter valueSearchEntityField valueSearchModelField filt searchModel entityList =
  let maybeValueSearchModelField = valueSearchModelField searchModel
   in ff maybeValueSearchModelField valueSearchEntityField filt entityList

ff :: Maybe b -> (a -> b) -> (b -> b -> Bool) -> [a] -> [a]
ff (Just value) valueSearchEntityField filt entityList = filter (\ent -> filt (valueSearchEntityField ent) value) entityList
ff Nothing _ _ entityList = entityList

pagination :: Int -> Int -> [a] -> [a]
pagination page sizePage = take sizePage . drop ((page - 1) * sizePage)