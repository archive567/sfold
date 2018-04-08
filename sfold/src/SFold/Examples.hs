{-# LANGUAGE OverloadedStrings #-}

module SFold.Examples where

import Control.Category ((.))
import qualified Control.Foldl as Foldl
import Control.Monad
import Data.Monoid
import Pipes hiding (X)
import qualified Pipes.Prelude as Pipes
import Protolude hiding ((.), (<>))
import SFold (SFold)
import qualified SFold
import SFold.SBar
import qualified SFold.Util as SFold

-- example 2
stream1 :: (Monad m) => Producer Int m ()
stream1 = Pipes.each [4, 5, 10, 12, 0, 0, 42]

foldSummary :: Int -> SFold.SFold Int Int
foldSummary n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then (0, snd x ++ snd x' ++ [acc])
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ [fst x])

showStream' :: SFold.SFold Int Int -> Producer Int IO () -> IO ()
showStream' f p =
  runEffect $
  SFold.sfold f (p >-> Pipes.chain (\x -> putStrLn $ "orig: " ++ show x)) >->
  Pipes.concat >->
  Pipes.chain (\x -> putStrLn $ "fold: " ++ show x) >->
  forever await

-- showStream' (foldSummary 10) stream1
-- variation: multiples of 10
foldMultiples :: Int -> SFold.SFold Int Int
foldMultiples n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then let y = (acc `div` n) * n
                in (acc - y, snd x ++ snd x' ++ [y])
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ [fst x])

tMult :: [Int] -> IO ()
tMult xs =
  runEffect $ SFold.sfold (foldMultiples 10) (Pipes.each xs) >-> Pipes.print

-- variation: 10s
foldTens :: Int -> SFold.SFold Int Int
foldTens n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then let y = acc `div` n
                in (acc - y * n, snd x ++ snd x' ++ replicate y n)
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ replicate (fst x `div` n) n)

tTens :: [Int] -> IO ()
tTens xs = runEffect $ SFold.sfold (foldTens 10) (Pipes.each xs) >-> Pipes.print

-- some deconstruction
t' :: [Int] -> IO ()
t' xs =
  runEffect $
  SFold.sfold (foldMultiples 100 . foldSummary 10) (Pipes.each xs) >->
  Pipes.print

t10 :: [Int] -> IO ()
t10 xs =
  runEffect $ SFold.sfold (foldSummary 10) (Pipes.each xs) >-> Pipes.print

t100 :: [Int] -> IO ()
t100 xs =
  runEffect $ SFold.sfold (foldSummary 100) (Pipes.each xs) >-> Pipes.print

main :: IO ()
main = t' [100]

newtype X =
  X (Int, [Int])
  deriving (Show)

instance Monoid X where
  mempty = X (0, [])
  mappend (X x) (X x') = X (fst x + fst x', snd x ++ snd x')

to' :: Int -> X
to' a = X (a, [])

begin' :: X
begin' = X (0, [])

step' :: Int -> X -> X -> X
step' n (X x) (X x') =
  let acc = fst x + fst x'
  in if acc >= n
       then X (0, snd x ++ snd x' ++ [acc])
       else X (acc, snd x ++ snd x')

release' :: X -> (X, [Int])
release' (X x) = (X (fst x, []), snd x)

flush' :: X -> X
flush' (X x) =
  if fst x == 0
    then X (0, snd x)
    else X (0, snd x ++ [fst x])

flushr' :: [Int] -> X
flushr' = foldr (step' 100 . to') (mempty :: X)

to'' :: Int -> (X, X)
to'' = second flushr' . release' . flush' . to'

step'' :: (X, X) -> (X, X) -> (X, X)
step'' (x0, x1) (x0', x1') = (step' 10 x0 x0', step' 100 x1 x1')

begin'' :: (X, X)
begin'' = (mempty :: X, mempty :: X)

release'' :: (X, X) -> ((X, X), [Int])
release'' (x0, x1) =
  let (x0', out) = release' x0
      x1' = step' 100 x1 (flushr' out)
      (x1'', out') = release' x1'
  in ((x0', x1''), out')

flush'' :: (X, X) -> (X, X)
flush'' (x0, x1) =
  let x0' = flush' x0
      (x0'', out) = release' x0'
      x1' = step' 100 x1 (flushr' out)
      x1'' = flush' x1'
  in (x0'', x1'')

-- example 1
stream1' :: (Monad m) => Producer Int m ()
stream1' = Pipes.each [4, 5, 2, 1, 8, 0, 4, 3]

foldlSummary :: Int -> Foldl.Fold Int [Int]
foldlSummary n = Foldl.Fold step begin done
  where
    begin = (0 :: Int, [])
    step x a =
      let acc = fst x + a
      in if acc >= n
           then (0, snd x ++ [acc])
           else (acc, snd x)
    done (acc, res) = res ++ [acc]

showStream :: Foldl.Fold Int [Int] -> Producer Int IO () -> IO ()
showStream f p =
  runEffect $
  p >-> Pipes.chain (\x -> putStrLn $ "orig: " ++ show x) >->
  Foldl.purely Pipes.scan f >->
  Pipes.chain (\x -> putStrLn $ "fold: " ++ show x) >->
  forever await

example1 :: IO ()
example1 = showStream (foldlSummary 10) stream1

-- example 2
foldSummary' :: Int -> SFold.SFold Int Int
foldSummary' n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then (0, snd x ++ snd x' ++ [acc])
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ [fst x])

showStream'' :: SFold.SFold Int Int -> Producer Int IO () -> IO ()
showStream'' f p =
  runEffect $
  SFold.sfold f (p >-> Pipes.chain (\x -> putStrLn $ "orig: " ++ show x)) >->
  Pipes.concat >->
  Pipes.chain (\x -> putStrLn $ "fold: " ++ show x) >->
  forever await

-- showStream' (foldSummary 10) stream1
-- variation: multiples of 10
foldMultiples' :: Int -> SFold.SFold Int Int
foldMultiples' n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then let y = (acc `div` n) * n
                in (acc - y, snd x ++ snd x' ++ [y])
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ [fst x])

-- showStream' (foldMultiples 10) stream1
-- variation: 10s
foldTens' :: Int -> SFold.SFold Int Int
foldTens' n = SFold.SFold to step begin release flush
  where
    to a = (a, [])
    begin = (0, [])
    step x x' =
      let acc = fst x + fst x'
      in if acc >= n
           then let y = acc `div` n
                in (acc - y * n, snd x ++ snd x' ++ replicate y 1)
           else (acc, snd x ++ snd x')
    release x = ((fst x, []), snd x)
    flush x =
      if fst x == 0
        then (0, snd x)
        else (0, snd x ++ replicate (fst x `div` n) 1)

-- showStream' (foldTens 10) stream1
-- showStream' (foldSummary 2 . foldMultiples 6 . foldTens 3) stream1
example2 :: IO ()
example2 = showStream' (foldSummary 2 . foldMultiples 6 . foldTens 3) stream1

-- example 1
stream1'' :: (Monad m) => Producer Double m ()
stream1'' = Pipes.each [4, 5, 10, 12, 0, 0, 42]

foldSummary'' :: Double -> Foldl.Fold Double [Double]
foldSummary'' n = Foldl.Fold step begin done
  where
    begin = (0 :: Double, [])
    step x a =
      let acc = fst x + a
      in if acc >= n
           then (0, snd x ++ [acc])
           else (acc, snd x)
    done = snd

ex1 :: (Monad m) => Producer Double m () -> m [Double]
ex1 = Foldl.purely Pipes.fold (foldSummary'' 10)

data VolumeTime = VolumeTime
  { volume :: Double
  , time :: Double
  } deriving (Show)

toTuple :: VolumeTime -> (Double, Double)
toTuple vt = (volume vt, time vt)

exampleData :: [VolumeTime]
exampleData =
  uncurry VolumeTime <$> [(23, 1), (2, 2), (3, 2), (5, 6), (7, 8), (34, 10)]

exampleStream :: (Monad m) => Producer VolumeTime m ()
exampleStream = each exampleData

-- accumulator
data X' = X'
  { xValue :: Double -- value accumulation (proxy is volume)
  , xFirstTime :: Maybe Double
  , xLastTime :: Maybe Double -- first and last time will be needed to track when to convert the value accumulation to summary data
  , xResult :: [VolumeTime] -- summary data
  } deriving (Show)

instance Monoid X' where
  mempty = X' 0 Nothing Nothing []
  mappend (X' v0 ft0 lt0 r0) (X' v1 ft1 lt1 r1) = X' v ft lt r2
    where
      v = v0 + v1
      ft = maybeMappend min ft0 ft1
      lt = maybeMappend max lt0 lt1
      r2 = r0 ++ r1

maybeMappend :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMappend ma x x' =
  case x of
    Nothing ->
      case x' of
        Nothing -> Nothing
        Just v' -> Just v'
    Just v ->
      case x' of
        Nothing -> Just v
        Just v' -> Just (ma v v')

foldlTime :: Double -> Foldl.Fold VolumeTime [VolumeTime]
foldlTime interval = Foldl.Fold step begin done
  where
    begin = mempty
    to (VolumeTime v t) = X' v (Just t) (Just t) []
    step' x x' =
      let x'' = x <> x'
      in if trigger x''
           then flush x <> x'
           else x''
    step x a = step' x (to a)
    trigger (X' _ ft' lt' _) =
      fromMaybe
        False
        ((\ft lt i -> (lt - ft) >= i) <$> ft' <*> lt' <*> pure interval)
    flush x@(X' v' _ lt' r') =
      case lt' of
        Nothing -> x
        Just lt -> X' 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    done = xResult . flush

fold1 :: Double -> SFold VolumeTime VolumeTime
fold1 interval = SFold.SFold to step mempty release flush
  where
    to (VolumeTime v t) = X' v (Just t) (Just t) []
    step x x' =
      let x'' = x <> x'
      in if trigger x''
           then bud x <> x'
           else x''
    trigger (X' _ ft' lt' _) =
      fromMaybe
        False
        ((\ft lt i -> (lt - ft) >= i) <$> ft' <*> lt' <*> pure interval)
    bud x@(X' v' _ lt' r') =
      case lt' of
        Nothing -> x
        Just lt -> X' 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    release (X' v' ft' lt' r') = (X' v' ft' lt' [], r')
    flush = bud

showStream''' ::
     MonadIO m
  => Producer VolumeTime m ()
  -> SFold VolumeTime VolumeTime
  -> m ()
showStream''' stream sf =
  runEffect $
  (stream >->
   Pipes.tee
     (Pipes.map ((("orig:" :: Text) <>) . show . toTuple) >-> Pipes.print)) SFold.>?>
  sf >->
  Pipes.map ((("summ:" :: Text) <>) . show . toTuple) >->
  Pipes.print

example3 :: IO ()
example3 = do
  let t1 = [(23, 1), (2, 2), (3, 2), (5, 6), (7, 8), (34, 10)]
  runEffect $
    SFold.sfold (valueFold 10 . timeFold 3) (Pipes.each t1) >-> Pipes.print
