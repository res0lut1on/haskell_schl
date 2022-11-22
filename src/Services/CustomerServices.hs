import Data.Entities (Customer (customerId), Order (orderId))
import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel)
import Repositories.CustomersRepository (getCustomerById)
import qualified Repositories.CustomersRepository as CustomersRepository
import Repositories.OrderRepository

getModelCustomers :: [CustomerModel]
getModelCustomers = map (`mappingCustomerToModel` Nothing) CustomersRepository.getCustomers

getModelCustomerById :: Int -> Maybe CustomerModel
getModelCustomerById custId =
  let orders = Just $ getOrdersByCustomerId custId
      customer = getCustomerById custId
   in case customer of
        Nothing -> Nothing
        Just value -> Just $ mappingCustomerToModel value orders