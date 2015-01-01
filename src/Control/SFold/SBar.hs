{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Control.SFold.SBar where

import Control.SFold
import Control.SFold.Util
import           Control.Applicative
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))

data SBar = SBar {sbarValue::Double,sbarFirstTime::Maybe Double,sbarLastTime::Maybe Double,sbarResult::[(Double,Double)]} deriving (Show)

timeFold :: Double -> SFold (Double, Double) (Double, Double)
timeFold interval = predFold $ timeBud interval

valueFold :: Double -> SFold (Double, Double) (Double, Double)
valueFold maxVal = predFold $ valueBud maxVal

predFold :: (SBar -> SBar -> Bool) -> SFold (Double, Double) (Double, Double)
predFold budSBar = SFold to ma state' sweep flush
  where
    to (v,t) = SBar v (Just t) (Just t) []
    me = SBar 0 Nothing Nothing []
    ma s0@(SBar v0 ft0 lt0 out0) s1@(SBar v1 ft1 lt1 out1) =
      bar
      where
        v = v0+v1
        ft = orMaybe id min ft0 ft1
        lt = orMaybe id max lt0 lt1
        out = out0 <> out1
        bar = if budSBar s0 s1
              then SBar 0 lt Nothing (out <> [(v,fromJust lt)])
              else SBar v ft lt out
    state' = me
    sweep (SBar v ft lt out) = (SBar v ft lt [], out)
    flush b@(SBar v _ lt out) = if v>0
                                then SBar 0 lt Nothing (out <> [(v,fromJust lt)])
                                else b

valueBud :: Double -> SBar -> SBar -> Bool
valueBud m (SBar v0 _ _ _) (SBar v1 _ _ _) = (v0+v1) >= m

timeBud :: Double -> SBar -> SBar -> Bool
timeBud interval (SBar _ ft0 lt0 _) (SBar _ ft1 lt1 _) =
    case (-) <$> lt <*> ft of
        Nothing -> False
        Just x -> x >= interval
  where
  ft = orMaybe id min ft0 ft1
  lt = orMaybe id max lt0 lt1
  
-- t1 :: [(Double,Double)]
-- t1 = [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]
-- fold (toFold (compSFold (timeFold 3) (valueFold 10))) t1
