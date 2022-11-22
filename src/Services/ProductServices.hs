import Data.Entities
import Data.Models (ProductModel)
import Mapping.Mapping
import Repositories.ProductRepository (getProductById)
import qualified Repositories.ProductRepository
import Repositories.ShopRepository

getModelProducts :: [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) Repositories.ProductRepository.getProducts

getModelProductById :: Int -> Maybe ProductModel
getModelProductById prID =
  let prod = getProductById prID
   in case prod of
        Nothing -> Nothing
        Just value -> Just $ mappingProductToModel value (getShopById $ productShopId value)
