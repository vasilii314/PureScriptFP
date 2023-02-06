module Ch7Csv
  where

import Prelude

import Data.Generic.Rep (class Generic, NoArguments(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)


test :: Effect Unit
test = do
  log $ show $ toCsv (Person { name: FullName "Joe", age: Age 29, occupation: Dentist })


newtype CSV = CSV String
derive instance newCSV :: Newtype CSV _
instance showCSV :: Show CSV where
  show csv = unwrap csv

class ToCsv a where
  toCsv :: a -> CSV

newtype FullName = FullName String
derive instance newTypeFullName :: Newtype FullName _
instance showFullName :: Show FullName where
  show n = unwrap n

newtype Age = Age Int
derive instance newTypeAge :: Newtype Age _
instance showAge :: Show Age where
  show a = show $ unwrap a

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

instance toCsvPerson :: ToCsv Person where
  toCsv (Person p) = CSV (show (p.name) <> "," <> show (p.age) <> "," <> show (p.occupation)) 


class FromCSV a where
  fromCSV :: CSV -> Maybe a

