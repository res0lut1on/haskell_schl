module Services.EntityServices
  ( getModelShops,
    getModelShopById,
    addModelShop,
    removeModelShop,
    editModelShop,
    getModelProducts,
    getModelProductById,
    addModelProduct,
    removeModelProduct,
    editModelProduct,
    getOrders,
    getModelOrderById,
    addModelOrder,
    removeModelOrder,
    editModelOrder,
    getModelCustomers,
    getModelCustomerById,
    addModelCustomer,
    removeModelCustomer,
    editModelCustomer,
  )
where

import Services.CustomerServices (addModelCustomer, editModelCustomer, getModelCustomerById, getModelCustomers, removeModelCustomer)
import Services.OrderServices (addModelOrder, editModelOrder, getModelOrderById, getOrders, removeModelOrder)
import Services.ProductServices (addModelProduct, editModelProduct, getModelProductById, getModelProducts, removeModelProduct)
import Services.ShopServices (addModelShop, editModelShop, getModelShopById, getModelShops, removeModelShop)
