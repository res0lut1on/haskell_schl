module Data.Context
  ( сustomers,
    products,
    shops,
    orders,
    productOrder,
  )
where

import Data.CommonData (Color (..))
import Data.Entities (Customer (..), Order (..), Product (..), ProductOrder (..), Shop (..))

сustomers :: [Customer]
сustomers =
  [ Customer
      { customerId = 1,
        customerName = "Michel",
        customerAddress = "Gary, Indiana"
      },
    Customer
      { customerId = 2,
        customerName = "Jimmy",
        customerAddress = "Heston, UK"
      },
    Customer
      { customerId = 3,
        customerName = "David",
        customerAddress = "Cambrige, UK"
      }
  ]

shops :: [Shop]
shops =
  [ Shop
      { shopId = 1,
        shopName = "Green",
        shopAddress = "Vulitsa Vanyeyeva 12"
      },
    Shop
      { shopId = 2,
        shopName = "Auchan",
        shopAddress = " ulica Parkovaya 37"
      }
  ]

products :: [Product]
products =
  [ Product 1 2 "Milk" 3.90 White,
    Product 2 1 "Kabachok" 5.49 Other,
    Product 3 2 "Honey bunny" 9.99 Black
  ]

orders :: [Order]
orders =
  [ Order
      { orderId = 1,
        oCId = 1,
        oNumber = "ABC1"
      },
    Order
      { orderId = 2,
        oCId = 1,
        oNumber = "BBC2"
      },
    Order
      { orderId = 3,
        oCId = 3,
        oNumber = "BCA3"
      },
    Order
      { orderId = 4,
        oCId = 2,
        oNumber = "BDF4"
      }
  ]

productOrder :: [ProductOrder]
productOrder =
  [ ProductOrder
      { productOrderId = 1,
        poId = 2,
        opId = 2
      },
    ProductOrder
      { productOrderId = 2,
        poId = 3,
        opId = 1
      },
    ProductOrder
      { productOrderId = 3,
        poId = 1,
        opId = 3
      },
    ProductOrder
      { productOrderId = 4,
        poId = 2,
        opId = 3
      },
    ProductOrder
      { productOrderId = 5,
        poId = 2,
        opId = 4
      },
    ProductOrder
      { productOrderId = 6,
        poId = 2,
        opId = 4
      }
  ]