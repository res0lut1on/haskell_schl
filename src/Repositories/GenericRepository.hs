module Repositories.GenericRepository
  ( GenericRepository (..),
    getCustomerByOrder,
    getCustomerByOrderId,
    getOrderByCustomerId,
    getOrdersByCustomerId,
    getProductsByShop,
    getProductsByOrder,
    getProductsWithOrdersId,
    getProductsByOrderId,
  )
where

import Repositories.CustomerGR (getCustomerByOrder, getCustomerByOrderId)
import Repositories.GRepository
import Repositories.OrderGR (getOrderByCustomerId, getOrdersByCustomerId)
import Repositories.ProductGR (getProductsByOrder, getProductsByOrderId, getProductsByShop, getProductsWithOrdersId)
import Repositories.ProductOrderGR ()
import Repositories.ShopGR ()