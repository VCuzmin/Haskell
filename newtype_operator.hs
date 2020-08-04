newtype CustomerId = CustomerId Int

customerId = CustomerId 1

customerToInt :: CustomerId -> Int
customerToInt (CustomerId i) = i
