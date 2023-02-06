module Ch5
  ( ($)
  , apply
  , flip
  , myConst
  , myDropWhile
  , myRange
  , test
  )
  where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ myZip (Nil :: List Int) (Nil :: List String)
  -- log $ show $ myTakeEnd 2 (1 : 2 : 3 : 4 : Nil)
  -- log $ show $ myDropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  -- log $ show $ myDrop (-1) (1 : 3: 4 : Nil :: List Int)
  -- log $ show $ myRange 3 (-3)
  -- log $ show $ myCatMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  -- log $ show $ myFilter (_ > 2) (1 : 2 : 3 : 4 : Nil)
    -- case myFindLastIndex (_ > 100) (1 : 2 : 3 : 4 : Nil) of 
    --   Just x -> log $ show $ (1 : 2 : 3 : 4 : Nil) !! x
    --   Nothing -> log "Nothing" 

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip func x y = func y x

myConst :: forall a b. a -> b -> a 
myConst a _ = a

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply
infixl 1 applyFlipped as #


singleton :: forall a. a -> List a
singleton a = a : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

mysnoc :: forall a. List a -> a -> List a
mysnoc Nil a = a : Nil
mysnoc (x : xs) a = x : mysnoc xs a

mylength :: forall a. List a -> Int
mylength l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs 


myhead :: forall a. List a -> Maybe a
myhead Nil = Nothing
myhead (x : _) = Just x


myTail :: forall a. List a -> Maybe (List a)
myTail Nil = Nothing
myTail (_ : xs) = Just xs

myLast :: forall a. List a -> Maybe a
myLast Nil = Nothing
myLast (x : Nil) = Just x
myLast (_ : xs) = myLast xs

myInit :: forall a. List a -> Maybe (List a)
myInit Nil = Nothing
myInit l = Just (go l) where
  go :: List a -> List a
  go Nil = Nil
  go (_ : Nil) = Nil  
  go (x : xs) = x : go xs 


myUncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
myUncons Nil = Nothing
myUncons (x : xs) = Just { head: x, tail : xs }

myIndex :: forall a. List a -> Int -> Maybe a
myIndex Nil _ = Nothing
myIndex l i = go 0 l where
  go :: Int -> List a -> Maybe a
  go _ Nil = Nothing
  go ci (x : xs) = if ci == i then Just x else go (ci + 1) xs

infixr 8 myIndex as !!

myFindIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
myFindIndex _ Nil = Nothing
myFindIndex p l = go 0 l where
  go :: Int -> List a -> Maybe Int
  go _  Nil = Nothing
  go ci (x : xs) = if p x then Just ci else go (ci + 1) xs


myFindLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
myFindLastIndex _ Nil = Nothing
myFindLastIndex p l = go 0 (-1) p l where
  go :: Int -> Int -> (a -> Boolean) -> List a -> Maybe Int
  go _ fi _ Nil 
    | fi < 0 = Nothing
    | otherwise = Just fi
  go ci fi p (x : xs) = if p x then go (ci + 1) ci p xs else go (ci + 1) fi p xs


myReverse :: forall a. List a -> List a
myReverse Nil = Nil
myReverse l = go Nil l where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs 


myConcat :: forall a. List (List a) -> List a
myConcat Nil = Nil
myConcat (Nil : xs) = myConcat xs
myConcat ((x : xs) : xss) = x : myConcat (xs : xss)

myFilter :: forall a. (a -> Boolean) -> List a -> List a
myFilter _ Nil = Nil
myFilter p l = myReverse (go l Nil) where
  go :: List a -> List a -> List a
  go Nil acc = acc
  go (x : xs) acc = if p x then go xs (x : acc) else go xs acc 

myCatMaybes :: forall a. List (Maybe a) -> List a
myCatMaybes Nil = Nil
myCatMaybes l = myReverse (go Nil l) where
  go :: List a -> List (Maybe a) -> List a
  go acc Nil = acc
  go acc (x : xs) = case x of
    Just y -> go (y : acc) xs
    Nothing -> go acc xs


myRange :: forall a. Int -> Int -> List Int
myRange x y 
  | x == y = x : Nil
  | x < y = x : myRange (x + 1) y
  | otherwise = x : myRange (x - 1) y

myTake :: forall a. Int -> List a -> List a
myTake _ Nil = Nil
myTake n (x : xs)
  | n <= 0 = Nil
  | otherwise = x : myTake (n - 1) xs

myDrop :: forall a. Int -> List a -> List a
myDrop _ Nil = Nil
myDrop n (_ : xs) 
  | n < 0 = Nil
  | n == 1 = xs
  | otherwise = myDrop (n - 1) xs


myTakeWhile :: forall a. (a -> Boolean) -> List a -> List a
myTakeWhile _ Nil = Nil
myTakeWhile p l = myReverse (go l Nil) where
  go :: List a -> List a -> List a
  go Nil acc = acc
  go (x : xs) acc 
    | p x = go xs (x : acc)
    | otherwise = go Nil acc


myDropWhile :: forall a. (a -> Boolean) -> List a -> List a
myDropWhile _ Nil = Nil
myDropWhile p (x : xs) 
  | p x = myDropWhile p xs
  | otherwise = (x : xs)



-- myTakeEnd n l = go n l Nil where
--   go :: Int -> List a -> List a -> List a
--   go 

myZip :: forall a b. List a -> List b -> List (Tuple a b)
myZip _ Nil = Nil
myZip Nil _ = Nil
myZip ll lr = myReverse (go ll lr Nil) where
   go :: List a -> List b -> List (Tuple a b) -> List (Tuple a b)
   go _ Nil acc = acc
   go Nil _ acc = acc
   go (x : xs) (y : ys) acc = go xs ys (Tuple x y : acc)


myUnzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
myUnzip Nil = Tuple Nil Nil
myUnzip (Tuple x y : ts) = myUnzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)