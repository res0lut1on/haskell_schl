import Data.Entities (Shop (shopId))
import Data.Models (ShopModel)
import Mapping.Mapping (mappingShopToModel)
import Repositories.ProductRepository (getProductsByShop)
import Repositories.ShopRepository (getShopById)
import qualified Repositories.ShopRepository as ShopRepository

getModelShops :: [ShopModel]
getModelShops = map (`mappingShopToModel` Nothing) ShopRepository.getShops

getModelShopsById :: Int -> Maybe ShopModel
getModelShopsById smId =
  let shop = getShopById smId
   in case shop of
        Nothing -> Nothing
        Just value -> Just $ mappingShopToModel value (Just $ getProductsByShop value)