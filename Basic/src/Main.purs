module Main
  where

import Prelude

import Ch5 as Ch5
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Ch7 as Ch7
import Ch7Csv as Ch7Csv
import Ch9 as Ch9

type Address = 
    { street1 :: String
    , street2 :: String
    , city    :: String
    , state   :: String
    , zip     :: String
    }

data Person = Person
    { name    :: String
    , age     :: Int
    , address :: Address
    }

data Company = Company 
    { name    :: String
    , address :: Address
    }

data Residence = 
    Home Address
    | Facility Address    


class HasAddress a where
  getAddress :: a -> Address


instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home address) = address
  getAddress (Facility address) = address 
  

getDirections :: forall a. Show a => HasAddress a => a -> Address
getDirections hasAddr = getAddress hasAddr

instance showAddressResidence :: Show Residence where
  show (Home address) = show address
  show (Facility address) = show address


instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.address == p2.address


data SomeType = This | That | TheOther | AndYetAnother
derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType
derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

main :: Effect Unit
main = do
  Ch9.test
  -- log $ show $ getDirections $ Facility { street1: "Street 10", street2: "Street 20", city: "Moscow", state: "Russia", zip: "12345" }
