module Control.SFold.Example where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Applicative
import qualified Control.Foldl as Foldl
import Control.SFold.Util
import Control.SFold.SBar
import qualified Pipes.Prelude as Pipes
import Pipes hiding (X)
import Data.Maybe
import Control.SFold
import Data.Monoid
import Control.Monad

data VolumeTime = VolumeTime { volume :: Double, time :: Double} deriving (Show)

toTuple :: VolumeTime -> (Double, Double)
toTuple vt = (volume vt, time vt)

exampleData :: [VolumeTime]
exampleData = uncurry VolumeTime <$> [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]

exampleStream :: (Monad m) => Producer VolumeTime m ()
exampleStream = each exampleData

-- accumulator
data X = X
    { xValue     :: Double       -- value accumulation (proxy is volume)
    , xFirstTime :: Maybe Double 
    , xLastTime  :: Maybe Double -- first and last time will be needed to track when to convert the value accumulation to summary data
    , xResult    :: [VolumeTime] -- summary data
    } deriving (Show)

instance Monoid X where
    mempty = X 0 Nothing Nothing []
    mappend (X v0 ft0 lt0 r0) (X v1 ft1 lt1 r1) =
        X v ft lt r2
        where
          v = v0 + v1
          ft = maybeMappend min ft0 ft1
          lt = maybeMappend max lt0 lt1
          r2 = r0++r1

maybeMappend :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMappend ma x x' =
  case x of
    Nothing -> case x' of
      Nothing -> Nothing
      Just v' -> Just v'
    Just v -> case x' of
      Nothing -> Just v
      Just v' -> Just (ma v v')

foldlTime :: Double -> Foldl.Fold VolumeTime [VolumeTime]
foldlTime interval = Foldl.Fold step begin done
  where
    begin = mempty
    to (VolumeTime v t) = X v (Just t) (Just t) []
    step' x x' =
        let x'' = x <> x' in
        if trigger x'' then flush x <> x' else x''
    step x a = step' x (to a)
    trigger (X _ ft' lt' _) =
        fromMaybe False
        ((\ft lt i -> (lt-ft) >= i) <$> ft' <*> lt' <*> pure interval) 
    flush x@(X v' _ lt' r') =
        case lt' of
            Nothing -> x
            Just lt -> X 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    done = xResult . flush

foldlTime' :: Double -> Double -> Foldl.Fold VolumeTime [VolumeTime]
foldlTime' start interval = Foldl.Fold step begin done
  where
    begin = mempty {xLastTime = Just (start+interval)}
    to (VolumeTime v t) = X v Nothing (Just (nextInterval t)) []
    nextInterval t = fromIntegral $ ceiling (t / interval)
    step' x x' =
        let x'' = x <> x' in
        if trigger x x' then flush x <> x' else x''
    step x a = step' x (to a)
    trigger (X _ _ lt0 _) (X _ _ lt1 _) =
        fromMaybe False
        ((\lt0' lt1' -> (lt1' > lt0')) <$> lt0 <*> lt1)
    flush x@(X v' _ lt' r') =
        case lt' of
            Nothing -> x
            Just lt -> X 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    done = xResult . flush

fold1 :: Double -> SFold VolumeTime VolumeTime
fold1 interval = SFold to step mempty release flush
  where
    to (VolumeTime v t) = X v (Just t) (Just t) []
    step x x'=
        let x'' = x <> x' in
        if trigger x'' then bud x <> x' else x'' 
    trigger (X _ ft' lt' _) =
        fromMaybe False
        ((\ft lt i -> (lt-ft) >= i) <$> ft' <*> lt' <*> pure interval) 
    bud x@(X v' _ lt' r') =
        case lt' of
            Nothing -> x
            Just lt -> X 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    release (X v' ft' lt' r') = (X v' ft' lt' [], r')
    flush = bud

showStream :: MonadIO m => Producer VolumeTime m () -> SFold VolumeTime VolumeTime -> m ()
showStream stream sf =
    runEffect $
    (stream
     >-> Pipes.tee (Pipes.map (("orig:"<>) . show . toTuple) >-> Pipes.print))
    >?> sf
    >-> Pipes.map (("summ:"<>) . show . toTuple)
    >-> Pipes.print

main :: IO ()
main = do
    let t1 = [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]
    runEffect $ sfold (valueFold 10 . timeFold 3) (Pipes.each t1) >-> Pipes.print
