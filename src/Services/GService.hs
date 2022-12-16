{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Services.GService (GenericService (..)) where

import Data.Entities (Customer, Order, Product, Shop)
import Mapping.GMapping (GenericMapping (toList, toModel))
import Mapping.MappingParam (MappingParam (toModelParam))
import Repositories.CustomerGR ()
import Repositories.GRepository as R (GenericRepository (..))
import Repositories.OrderGR ()
import Repositories.ProductGR ()
import Repositories.ShopGR ()
import Services.SearchService (SearchService (..))
import Util.Utilities (unwrap)
import Startup

class (GenericRepository a) => GenericService a where
  getList :: (GenericMapping a b) => App [b]
  getList = toList <$> (R.getList :: App [a])

  get :: (MappingParam a b c) => (a -> App c) -> Int -> App (Maybe b)
  get getParams sid = unwrap $ (R.getEntityById sid :: App (Maybe a)) >>= \maybeEnt -> return $ maybeEnt >>= \ent -> return $ Just . toModelParam ent <$> getParams ent

  addModel :: (GenericMapping b a) => b -> App Int
  addModel tmodel = R.addEntity (toModel tmodel :: a)

  editModel :: (GenericMapping b a) => b -> App ()
  editModel tmodel = R.editEntity (toModel tmodel :: a)

  delete :: (GenericMapping a b) => Int -> App (Maybe b)
  delete sid =
    (removeEid sid :: App (Maybe a)) >>= \maybeEnt ->
      return (maybeEnt >>= \val -> return (toModel val))

  search :: (GenericMapping a b, SearchService c a) => c -> App [b]
  search model = toList <$> (R.search searchModel model :: App [a])

instance GenericService Product

instance GenericService Customer

instance GenericService Shop

instance GenericService Order
