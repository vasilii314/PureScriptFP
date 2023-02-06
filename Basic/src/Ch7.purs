module Ch7
  ( test
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

test :: Effect Unit
test = do
  log $ show (Left 7 :: Either _ Unit)
  -- log $ show (Person { name: "Josh", age: 29 } == Person { name: "Josh", age: 29 })
  -- log $ show (Just 7)
  -- log $ show $ Just 1 < Just 5
  -- log $ show $ Just 5 <= Just 5
  -- log $ show $ Just 5 > Just 10
  -- log $ show $ Just 10 >= Just 100
  -- log $ show $ Just 99 > Nothing
  -- log $ show $ Just 99 < Nothing

data Maybe a = Nothing | Just a

data Person = Person { name :: String, age :: Int }

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--  eq Nothing Nothing = false
--  eq (Just x) (Just y) = x == y
--  eq _ _ = false

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare (Just x) (Just y) = compare x y
--   compare _ Nothing = GT
--   compare Nothing _ = LT

-- greaterThanOrEq :: forall a. Ord a => Maybe a -> Maybe a -> Boolean
-- greaterThanOrEq x y = cmp == GT || cmp == EQ where cmp = compare x y

-- infixl 4 greaterThanOrEq as >=

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show (Just x) = "(Just " <> (show x) <> ")"
--   show Nothing = "Nothing"


derive instance genericMaybe :: Generic (Maybe a) _
instance name :: Show a => Show (Maybe a) where
  show = genericShow

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance compareMaybe :: Ord a => Ord (Maybe a)

derive instance eqPerson :: Eq Person

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance compareEither :: (Ord a, Ord b) => Ord (Either a b)
