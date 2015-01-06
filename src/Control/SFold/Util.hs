module Control.SFold.Util where

import           Control.Applicative
import qualified Control.Foldl as L
import           Control.SFold
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Monoid
import           Pipes
import Control.Monad

sfoldGet :: Monad m => (a -> x) -> (x -> x -> x) -> x -> (x -> (x,[b])) -> (x -> x) -> Producer a m () -> Producer [b] m ()
sfoldGet to step begin release flush p0 = loop p0 begin
  where
    loop p x = do
        n <- lift $ next p
        case n of
            Left r -> do
                yield $ snd . release . flush $ x
                return r
            Right (a, p') -> do
                let (x', bs) = release $ step x (to a)
                yield bs
                loop p' x'

sfold :: Monad m => SFold a b -> Producer a m () -> Producer [b] m ()
sfold (SFold to step begin release flush) =
    sfoldGet to step begin release flush

sfold' :: Monad m => SFold a b -> Producer a m () -> Producer b m ()
sfold' (SFold to step begin release flush) p =
    sfoldGet to step begin release flush p >-> flatten
  where
    flatten = forever $ do
        a <- await
        mapM_ yield a

(>?>) :: (Monad m) => Producer a m () -> SFold a b -> Producer b m ()
(>?>) p s = sfold' s p

-- pipes
sscan :: (Monad m) => SFold a b -> Pipe a b m r
sscan (SFold to step begin release _) = loop begin
  where
  loop st = do
    a <- await
    let (st',out) = release (step st (to a))
    mapM_ yield out
    loop st'

toFoldl :: SFold a b -> L.Fold a [b]
toFoldl (SFold to step begin release flush) =
  L.Fold step' begin done
  where
    step' x a = step x (to a)
    done = snd . release . flush

orMaybe :: (a -> b) -> (a -> a -> b) -> Maybe a -> Maybe a -> Maybe b
orMaybe unif binf x x' =
  case x of
    Nothing -> case x' of
      Nothing -> Nothing
      Just v' -> Just $ unif v'
    Just v -> case x' of
      Nothing -> Just $ unif v
      Just v' -> Just (binf v v')

orMaybe' :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
orMaybe' ma x x' =
  case x of
    Nothing -> case x' of
      Nothing -> Nothing
      Just v' -> Just v'
    Just v -> case x' of
      Nothing -> Just v
      Just v' -> Just (ma v v')

keyFold :: (Ord c) => SFold a b -> SFold (c, a) (c, b)
keyFold (SFold toX ma _ rel flush) = SFold to' ma' mempty rel' flush'
  where
    ma' = Map.unionWith ma
    to' (key, a) = Map.singleton key (toX a)
    rel' m = (newX, out)
      where
        relMap = Map.map rel m
        newX = Map.map fst relMap
        out = F.concat $ (\(sym,ms) -> (,) <$> pure sym <*> ms) <$>
              Map.toList (Map.map snd relMap)
    flush' = Map.map flush 

premap :: (a -> b) -> SFold b r -> SFold a r
premap f (SFold to step begin rel flush) = SFold to' step begin rel flush
  where
    to' = to . f
