module Control.SFold.Example where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Applicative
import qualified Control.Foldl as Foldl
import Control.SFold.Util
import Control.SFold.SBar
import qualified Pipes.Prelude as Pipes
import Pipes
import Data.Maybe
import Control.SFold
import Data.Monoid

data VolumeTime = VolumeTime { volume :: Double, time :: Double} deriving (Show)

toTuple :: VolumeTime -> (Double, Double)
toTuple vt = (volume vt, time vt)

exampleData :: [VolumeTime]
exampleData = uncurry VolumeTime <$> [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]

exampleStream :: (Monad m) => Producer VolumeTime m ()
exampleStream = each exampleData

-- provide a stream summarising every 3 seconds

-- accumulator
data AccTime =
    AccTime
    { acctValue :: Double
    , acctFirstTime :: Maybe Double
    , acctLastTime :: Maybe Double
    , acctResult :: [VolumeTime]
    } deriving (Show)

instance Monoid AccTime where
    mempty = AccTime 0 Nothing Nothing []
    mappend (AccTime v0 ft0 lt0 r0) (AccTime v1 ft1 lt1 r1) =
        AccTime v ft lt r2
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

fold1 :: Double -> SFold VolumeTime VolumeTime
fold1 interval = SFold to step mempty release flush
  where
    to (VolumeTime v t) = AccTime v (Just t) (Just t) []
    step x x'=
        let x'' = x <> x' in
        if trigger x'' then bud x <> x' else x'' 
    trigger (AccTime _ ft' lt' _) =
        fromMaybe False
        ((\ft lt i -> (lt-ft) >= i) <$> ft' <*> lt' <*> pure interval) 
    bud x@(AccTime v' _ lt' r') =
        case lt' of
            Nothing -> x
            Just lt -> AccTime 0 Nothing Nothing (r' ++ [VolumeTime v' lt])
    release (AccTime v' ft' lt' r') = (AccTime v' ft' lt' [], r')
    flush = bud

showStream :: MonadIO m => Producer VolumeTime m () -> SFold VolumeTime VolumeTime -> m ()
showStream stream sf =
    runEffect $
    (stream
     >-> Pipes.tee (Pipes.map (("orig:"<>) . show . toTuple) >-> Pipes.print))
    >?> sf
    >-> Pipes.map (("summ:"<>) . show . toTuple)
    >-> Pipes.print

data Release = Release Double

releaser :: (Monad m) => Double -> Producer VolumeTime m () -> Producer (Either Release VolumeTime) m ()
releaser = undefined

foldReleaser :: Double -> SFold VolumeTime VolumeTime -> SFold (Either Release VolumeTime) VolumeTime
foldReleaser interval (SFold to step begin release flush) = undefined

pipeReleaser :: (Monad m) => Producer VolumeTime m ()
pipeReleaser = releaser 3.0 exampleStream >?> foldReleaser 3.0 (fold1 3.0)

main :: IO ()
main = do
    let t1 = [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]
    runEffect $ sfold (valueFold 10 . timeFold 3) (Pipes.each t1) >-> Pipes.print