import Data.Entities
import Data.Models (ProductModel)
import Mapping.Mapping
import Repositories.ProductRepository (getProductById)
import qualified Repositories.ProductRepository
import Repositories.ShopRepository

getModelProducts :: [ProductModel]
getModelProducts = map (`mappingProductToModel` Nothing) Repositories.ProductRepository.getProducts

getModelProductById :: Int -> ProductModel
getModelProductById prID = mappingProductToModel (getProductById prID) (Just $ getShopById $ productShopId $ getProductById prID)