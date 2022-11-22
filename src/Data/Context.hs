module Data.Context
  ( сustomers,
    products,
    shops,
    orders,
    productOrder,
  )
where

import Data.CommonData (Color(..))
import Data.Entities (Customer(..), Order(..), Product(..), ProductOrder(..), Shop(..))

сustomers :: [Customer]
сustomers = [Customer 1 "Michel" "Gary, Indiana", Customer 2 "Jimmy" "Heston, UK", Customer 3 "David" "Cambrige, UK"]

shops :: [Shop]
shops = [Shop 1 "Green" "Vulitsa Vanyeyeva 12", Shop 2 "Auchan" " ulica Parkovaya 37"]

products :: [Product]
products = [Product 1 2 "Milk" 3.90 White, Product 2 1 "Kabachok" 5.49 Other, Product 3 2 "Honey bunny" 9.99 Black]

orders :: [Order]
orders = [Order 1 1 "ABC1", Order 2 1 "BBC2", Order 3 3 "BCA3", Order 4 2 "BDF4"]

productOrder :: [ProductOrder]
productOrder =
  [ ProductOrder 1 2 2,
    ProductOrder 2 3 1,
    ProductOrder 3 1 3
  ]