import Data.Models (ShopModel)
import Mapping.Mapping (mappingShopToModel)
import Repositories.ProductRepository (getProductsByShop)
import Repositories.ShopRepository (getShopById)
import qualified Repositories.ShopRepository as ShopRepository

getModelShops :: [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) ShopRepository.getShops

getModelShopsById :: Int -> ShopModel
getModelShopsById smId = mappingShopToModel (getShopById smId) (Just $ getProductsByShop $ getShopById smId)