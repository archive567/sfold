{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module SFold where

import Control.Arrow (second)
import Control.Category (Category(..))
import Data.Monoid (Monoid(..), (<>))
import Prelude hiding ((.), id)
import Data.Profunctor

{-| SFold stands for Stream Fold and is a representation of a fold of a Stream where:

    to: (a -> x) translates the incoming stream to the accumulator
    step: (x -> x -> x) is the accumulator step function

    This busts the usual step function for folds (x -> a -> x) into two (step x a ~ step x (to a).

    begin: x is the initial accumulator.

    release: (x -> (x,[b]) takes the accumulator and computes a new accumulator (a remainder say), and an accumulated result of the fold so far (a release).

    flush: (x -> x) prepare the accumulator for final release (eg on finalization of the stream)
-}
data SFold a b = forall x. SFold (a -> x) (x -> x -> x) x (x -> (x, [b])) (x -> x)

data Pair a b = Pair !a !b

instance Functor (SFold a) where
  fmap f (SFold to step begin release flush) =
    SFold to step begin (second (fmap f) . release) flush

instance Profunctor SFold where
    lmap f (SFold to step begin release flush) = SFold (to . f) step begin release flush
    rmap = fmap

instance Category SFold where
  id = SFold (: []) (<>) [] (\x -> ([], x)) id
  (SFold to1 step1 begin1 release1 flush1) . (SFold to0 step0 begin0 release0 flush0) =
    SFold to step begin release flush
    where
      flushr = foldr (step1 . to1) begin1 -- should be mempty
      to = second flushr . release0 . to0
      step (x0, x1) (x0', x1') = (step0 x0 x0', step1 x1 x1')
      begin = (begin0, begin1)
      release (x0, x1) =
        let (x0', out) = release0 x0
            x1' = step1 x1 (flushr out)
            (x1'', out') = release1 x1'
        in ((x0', x1''), out')
      flush (x0, x1) =
        let x0' = flush0 x0
            (x0'', out) = release0 x0'
            x1' = step1 x1 (flushr out)
            x1'' = flush1 x1'
        in (x0'', x1'')

instance Monoid b => Monoid (SFold a b) where
  mempty = SFold (const ()) (\() _ -> ()) () (\() -> ((), [])) (const ())
  mappend (SFold toL stepL beginL releaseL flushL) (SFold toR stepR beginR releaseR flushR) =
    SFold to step begin release flush
    where
      to a = Pair (toL a) (toR a)
      step (Pair xLL xRL) (Pair xLR xRR) = Pair (stepL xLL xLR) (stepR xRL xRR)
      begin = Pair beginL beginR
      release (Pair xL xR) =
        let (xL', bsL) = releaseL xL
            (xR', bsR) = releaseR xR
        in (Pair xL' xR', bsL <> bsR)
      flush (Pair xL xR) = Pair (flushL xL) (flushR xR)

instance Applicative (SFold a) where
    pure b = SFold (const ()) (\() _ -> ()) () (\() -> ((), [b])) (const ())
    (SFold toF stepF beginF releaseF flushF) <*> (SFold toA stepA beginA releaseA flushA) =
        SFold to step begin release flush
      where
        to a = Pair (toF a) (toA a)
        step (Pair xF xA) (Pair aF aA) = Pair (stepF xF aF)(stepA xA aA)
        begin = Pair beginF beginA
        release (Pair xF xA) =
            let (xA', outA) = releaseA xA
                (xF', outF) = releaseF xF
            in ((Pair xF' xA'), outF <*> outA)
        flush (Pair xF xA) = Pair (flushF xF) (flushA xA)

-- pure id <*> (SFold toA stepA beginA releaseA flushA)
-- (SFold (const ()) (\() _ -> ()) () (\() -> ((), [id])) (const ())) <*> (SFold toA stepA beginA releaseA flushA)
-- SFold (\x -> Pair () (toA x)) (\(Pair () xA) (Pair () aA) -> Pair () (stepA xA aA)) (Pair () beginA) (\(Pair () xA) -> ((Pair () (fst $ releaseA xA)), [id] <*> (snd $ releaseA xA))) (\(Pair () xA) -> Pair () flushA xA)
-- x is isomorphic to (Pair () x)
-- SFold (\x -> toA x) (\xA aA -> stepA xA aA) beginA (\xA -> (fst $ releaseA xA, [id] <*> (snd $ releaseA xA))) (\xA -> flushA xA)
-- lambda removal
-- SFold toA stepA beginA (\xA -> (fst $ releaseA xA, [id] <*> (snd $ releaseA xA))) flushA
-- [id] <*> is id
--  SFold toA stepA beginA (\xA -> (fst $ releaseA xA, (snd $ releaseA xA))) flushA
-- \x -> (fst $ f x, snd $ f x) is f
--  SFold toA stepA beginA releaseA flushA
--

