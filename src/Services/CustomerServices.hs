import Data.Models (CustomerModel)
import Mapping.Mapping (mappingCustomerToModel)
import Repositories.CustomersRepository (getCustomerById)
import qualified Repositories.CustomersRepository as CustomersRepository
import Repositories.OrderRepository (getOrdersByCustomer)

getModelCustomers :: [CustomerModel]
getModelCustomers = map (`mappingCustomerToModel` Nothing) CustomersRepository.getCustomers

getModelCustomerById :: Int -> CustomerModel
getModelCustomerById custId = mappingCustomerToModel (getCustomerById custId) (Just $ getOrdersByCustomer $ getCustomerById custId)