{-# LANGUAGE NoImplicitPrelude #-}

module Control.Foldl.Folds where

import Protolude hiding (head)
import           Control.Foldl (Fold(..))
import qualified Control.Foldl as L
import qualified Data.Map as Map hiding (foldr)
import Data.List (head, tail)

-- | turn a regular fold into a Map.Map fold
keyFold :: (Ord c) => Fold a b -> Fold (c, a) [(c, b)]
keyFold (Fold step begin done) = Fold step' begin' done'
  where
    step' x (key, a) =
      case Map.lookup key x of
        Nothing -> Map.insert key (step begin a) x
        Just x' -> Map.insert key (step x' a) x

    begin' = Map.empty
    done' x = Map.toList (Map.map done x)

-- | count instances using a supplied function to generate a key
countMap :: (Ord b) => (a -> b) -> L.Fold a (Map.Map b Int)
countMap key = L.Fold step begin done
  where
    begin = Map.empty
    done = identity
    step m x = Map.insertWith (+) (key x) 1 m

-- | fold counting number of keys
countK :: (Ord b) => (a -> b) -> Fold a (Map.Map b Int)
countK key = Fold step begin done
  where
    begin = Map.empty
    done = identity
    step m x = Map.insertWith (+) (key x) 1 m

-- | fold counting number of keys, given a filter
countKF :: (Ord b) => (a -> b) -> (a -> Bool) -> Fold a (Map.Map b Int)
countKF key filt = Fold step begin done
  where
    begin = Map.empty
    done = identity
    step m x = if filt x
                 then Map.insertWith (+) (key x) 1 m
                 else m

-- | first difference
delta :: (Num a) => Fold a (Maybe a)
delta = Fold step (Nothing, Nothing) done
  where
    step (p', _) p = (Just p, p')
    done (Just p1, Just p2) = Just $ p1 - p2
    done _ = Nothing

-- | return (geometric first difference)
ret :: Fold Double (Maybe Double)
ret = Fold step (Nothing, Nothing) done
  where
    step (p', _) p = (Just p, p')
    done (Just p1, Just p2) = Just $ (p1 / p2) - 1
    done _ = Nothing

-- | lag n
lag :: Int -> Fold a (Maybe a)
lag l = Fold step (0, []) done
  where
    step (c, h) a
      | c > l = (c + 1, tail h ++ [a])
      | otherwise = (c + 1, h ++ [a])
    done (c, h)
      | c > l = Just $ head h
      | otherwise = Nothing

-- list folds
count :: Fold a Integer
count = Fold (const . (1 +)) 0 identity

-- turn a regular fold into a list one
listify :: Int -> Fold a b -> Fold [a] [b]
listify n (Fold step' begin' done') = Fold stepL beginL doneL
  where
    stepL = zipWith step'
    beginL = replicate n begin'
    doneL = map done'

-- | tuple 2 folds
tuplefy :: Fold a b -> Fold c d -> Fold (a, c) (b, d)
tuplefy (Fold step0 begin0 done0) (Fold step1 begin1 done1) = Fold stepL beginL doneL
  where
    stepL (a0, a1) (x0, x1) = (step0 a0 x0, step1 a1 x1)
    beginL = (begin0, begin1)
    doneL (x0, x1) = (done0 x0, done1 x1)
