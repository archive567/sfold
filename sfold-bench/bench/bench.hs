{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import qualified Control.Foldl as Foldl
import Control.Monad.Identity
import Control.Monad.Trans.State.Strict
import Criterion
import Criterion.Measurement
import Criterion.Types
import qualified Data.Foldable as F
import qualified Data.Machine as M
import Data.Machine.Mealy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Tuple (swap)
import Formatting
import Options.Generic
import Perf
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as Pipes
import Protolude hiding ((%), evalStateT, expt, get, put)
import System.Environment
import Data.Scientific
import Data.TDigest

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  , sumsTo :: Maybe [Int] -- <?> "sum to these numbers"
  } deriving (Generic, Show)

instance ParseField [Int]

instance ParseRecord Opts

-- | compute deciles
--
-- > c5 <- decile 5 <$> ticks n f a
--
deciles :: (Functor f, Foldable f) => Int -> f Cycle -> [Double]
deciles n xs =
  (\x -> fromMaybe 0 $ quantile x (tdigest (fromIntegral <$> xs) :: TDigest 25)) <$>
  ((/ fromIntegral n) . fromIntegral <$> [0 .. n]) :: [Double]

-- | compute a percentile
--
-- > c <- percentile 0.4 . fst <$> ticks n f a
--
percentile :: (Functor f, Foldable f) => Double -> f Cycle -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)

expt' :: Int -> Format r (Scientific -> r)
expt' x = scifmt Exponent (Just x)

-- an (explicit state) left step to kick things off
type Step x a b = x -> a -> (b, x)

myStep :: Step Int Int [Int]
myStep x a =
  let x' = x + a
  in if x' >= 10
       then ([x'], 0)
       else ([], x')

-- machines
myMealy :: Mealy Int [Int]
myMealy = unfoldMealy myStep 0

mealy' :: [Int] -> [[Int]]
mealy' ns = runIdentity $ M.runT $ M.supply ns (M.auto myMealy)

-- pipes
data Mealy' a b =
  forall x. Mealy' x
                   (x -> a -> (b, x))

myMealy' :: Mealy' Int [Int]
myMealy' = Mealy' 0 myStep

toPipe :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe (Mealy' begin step) = go begin
  where
    go x = do
      a <- await
      let (b, x') = step x a
      yield b
      go x'

pipe :: [Int] -> [[Int]]
pipe ns = Pipes.toList (each ns >-> toPipe myMealy')

-- pipes using state (lifting state)
toPipe' :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe' (Mealy' begin step) =
  flip evalStateT begin $
  distribute $
  forever $ do
    a <- await
    x <- lift get
    let (b, x') = step x a
    yield b
    lift $ put x'

pipe' :: [Int] -> [[Int]]
pipe' ns = Pipes.toList (each ns >-> toPipe' myMealy')

-- pipes using state (lifting pipes)
toPipe'' :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe'' (Mealy' begin step) =
  flip evalStateT begin $
  forever $ do
    a <- lift await
    x <- get
    let (b, x') = step x a
    lift $ yield b
    put x'

pipe'' :: [Int] -> [[Int]]
pipe'' ns = Pipes.toList (each ns >-> toPipe'' myMealy')

-- just a Foldl.fold
toFoldl :: Mealy' a b -> Foldl.Fold a [b]
toFoldl (Mealy' begin step) = Foldl.Fold step' begin' done'
  where
    begin' = ([], begin)
    step' (output, acc) a = (\(b, acc') -> (b : output, acc')) $ step acc a
    done' = reverse . fst

foldl'' :: [Int] -> [[Int]]
foldl'' = Foldl.fold (toFoldl myMealy')

-- escaping skolems
data Mealy'' a b x =
  Mealy'' x
          (a -> x -> (x, b))

myMealy'' :: Mealy'' Int [Int] Int
myMealy'' = Mealy'' 0 (\a x -> swap (myStep a x))

toPipeWithSkolem ::
     (Monad m, Ord a, Num a) => Mealy'' a [a] a -> Pipe a [a] m ()
toPipeWithSkolem (Mealy'' begin step) = go begin
  where
    go x = do
      a <- await
      let (x', b) = step a x
      yield b
      go x'

skolem :: [Int] -> [[Int]]
skolem ns = Pipes.toList (each ns >-> toPipeWithSkolem myMealy'')

-- foldl'
-- myStep without an output tape
myStep' :: ([[Int]], Int) -> Int -> ([[Int]], Int)
myStep' (out, acc) a =
  let acc' = acc + a
  in if acc' >= 10
       then ([acc'] : out, 0)
       else ([] : out, acc')

foldz :: [Int] -> [[Int]]
foldz ns = fst $ F.foldl' myStep' ([], 0) ns

--criterion helpers
data Speed = Speed
  { _speedMutator :: Double
  , _speedGc :: Double
  } deriving (Show)

formatRun :: [Cycle] -> Text -> Text
formatRun cs label =
  sformat
    ((right 24 ' ' %. stext) % stext % (left 7 ' ' %. expt' 3) % " cycles")
    label
    (Text.intercalate " " $ sformat (left 7 ' ' %. expt' 3) <$> (\x -> scientific (fromIntegral x) 0) <$> take 5 cs)
    (fromFloatDigits $ percentile 0.4 cs)

formatRunHeader =
  sformat
    ((right 24 ' ' %. stext) % (left 7 ' ' %. stext) % (left 8 ' ' %. stext) %
     (left 8 ' ' %. stext) %
     (left 8 ' ' %. stext) %
     (left 8 ' ' %. stext) %
     (left 8 ' ' %. stext))
    "run"
    "first"
    "2nd"
    "3rd"
    "4th"
    "5th"
    "40th %"

run label t = (`formatRun` label) . fst <$> t

code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

speed' ::
     (Integral t)
  => Control.DeepSeq.NFData b =>
       t -> (a -> b) -> a -> IO Speed
speed' nSamples f a = do
  (m, _) <- Criterion.Measurement.measure (nf f a) (fromIntegral nSamples)
  return $ (\x -> Speed (measMutatorCpuSeconds x) (measGcCpuSeconds x)) m

render' :: Text -> Int -> Speed -> Text
render' label n speed =
  sformat
    (stext % "\t" % expt' 2 % "\t" % expt' 2 % "\t" % expt' 2 % "\t" % expt' 2 %
     "\n")
    label
    ((\x -> scientific (fromIntegral x) 0) n)
    (fromFloatDigits $ _speedMutator speed)
    (fromFloatDigits $ _speedGc speed)
    (fromFloatDigits $ (_speedGc speed + _speedMutator speed) / fromIntegral n)

-- speed test
main :: IO ()
main = do
  putStrLn ("bench fired up!" :: Text)
  o :: Opts <- getRecord "a random bit of text"
  let ns = fromMaybe 1000 (runs o)
  let n = fromMaybe 1000 (sumTo o)
  let t = ns * n
  let str = replicate n 1
  rmachines <- run "machines" $ ticks ns mealy' [1 .. n]
  rpipe <- run "pipe" $ ticks ns pipe [1 .. n]
  rpipestate <- run "pipe - state" $ ticks ns pipe'' [1 .. n]
  rpipebadstate <- run "pipe - bad state" $ ticks ns pipe' [1 .. n]
  rfoldl <- run "foldl" $ ticks ns foldl'' [1 .. n]
  rskolem <- run "pipe & skolems" $ ticks ns skolem [1 .. n]
  rjustfold <- run "just a fold" $ ticks ns foldz [1 .. n]
  writeFile "other/bench.md" $
    code
      [rmachines, rpipe, rpipestate, rpipebadstate, rfoldl, rskolem, rjustfold]

-- machines vs pipes
producerToSource :: Monad m => Producer b m r -> M.PlanT k b m r
producerToSource p =
  runEffect $
  hoist lift p >->
  forever
    (do a <- await
        lift $ M.yield a)

source :: (Monad m) => M.MachineT m k Int
source = M.construct (producerToSource (each [1 .. 100]))

summary :: (Monad m) => M.MachineT m k [Int]
summary = M.cap (M.auto myMealy) source

-- waits until machine runs completely
badPrint :: Int -> IO ()
badPrint n = print =<< (take n <$> M.runT summary)

prodTee :: (Monad m) => M.MachineT (Producer a m) (M.Is a) ()
prodTee =
  void $
  M.repeatedly $ do
    a <- M.await
    lift . yield $ a
    M.yield ()

fromProcess :: (Monad m) => Producer a m ()
fromProcess = M.runT_ prodTee

t1 :: IO ()
t1 =
  void $
  runEffect $
  void (M.runT (summary M.~> prodTee)) >-> Pipes.take 10 >-> Pipes.print

-- machines versus pipes
printMachine :: (Show a) => M.MachineT IO (M.Is [a]) ()
printMachine = M.repeatedly $ M.await >>= lift . print >> M.yield ()

machinePrint :: [Int] -> IO ()
machinePrint ns = M.runT_ $ M.supply ns (M.auto myMealy) M.~> printMachine

printPipe :: (Show a) => Consumer a IO ()
printPipe = forever $ await >>= lift . print

pipesPrint :: [Int] -> IO ()
pipesPrint ns = runEffect $ each ns >-> toPipe myMealy' >-> printPipe
