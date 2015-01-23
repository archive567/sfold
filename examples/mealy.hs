{-# LANGUAGE ExistentialQuantification #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}

module Main where

-- import Control.Category hiding ((.))
import           Control.Applicative
import           Control.DeepSeq
import qualified Control.Foldl as Foldl
import           Control.Monad.Identity
import           Control.Monad.Trans.State.Strict
import           Criterion
import           Criterion.Measurement
import           Criterion.Types
import qualified Data.Foldable as F
import qualified Data.Machine as M
import           Data.Machine.Mealy
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.Tuple (swap)
import           Formatting
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude as Pipes
import           System.Environment
import Data.Monoid

-- a concrete (explicit state) reducer to kick things off
-- x -> a -> (b,x) (adopting the unfoldMealy order)
myStep :: (Ord t, Num t) => t -> t -> t -> ([t],t)
myStep trigger acc a =
  let acc' = acc + a in
  if acc' >= trigger
  then ([acc'],0)
  else ([],acc')


-- machines
myMealy :: (Ord a, Num a) =>  a -> Mealy a [a]
myMealy trigger = unfoldMealy (myStep trigger) 0

mealy' :: (Ord a, Num a) => [a] -> [[a]]
mealy' ns = runIdentity $ M.runT $ M.supply ns (M.auto $ myMealy 10)

-- pipes
data Mealy' a b = forall x . Mealy' x (x -> a -> (b,x))

myMealy' :: (Ord a, Num a) => a -> Mealy' a [a]
myMealy' trigger = Mealy' 0 (myStep trigger)

toPipe :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe (Mealy' begin step) =
  go begin
  where
    go x = do
      a <- await
      let (b,x') = step x a
      yield b
      go x'

pipe :: (Ord a, Num a, Foldable f) => f a -> [[a]]
pipe ns = Pipes.toList (each ns >-> toPipe (myMealy' 10)) 

-- pipes using state (lifting state)
toPipe' :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe' (Mealy' begin step) =
  flip evalStateT begin $ distribute $ forever $ do
    a <- await
    x <- lift get
    let (b,x') = step x a
    yield b
    lift $ put x'

pipe' :: (Ord a, Num a, Foldable f) => f a -> [[a]]
pipe' ns = Pipes.toList (each ns >-> toPipe' (myMealy' 10)) 

-- pipes using state (lifting pipes)
toPipe'' :: (Monad m, Ord a, Num a) => Mealy' a [a] -> Pipe a [a] m ()
toPipe'' (Mealy' begin step) =
  flip evalStateT begin $ forever $ do
    a <- lift await
    x <- get
    let (b,x') = step x a
    lift $ yield b
    put x'

pipe'' :: (Ord a, Num a, Foldable f) => f a -> [[a]]
pipe'' ns = Pipes.toList (each ns >-> toPipe'' (myMealy' 10)) 

-- just a Foldl.fold
toFoldl :: Mealy' a b -> Foldl.Fold a [b]
toFoldl (Mealy' begin step) = Foldl.Fold step' begin' done'
  where
    begin' = ([],begin)
    step' (output,acc) a = (\(b,acc') -> (b:output,acc')) $ step acc a
    done' = reverse . fst

foldl' :: (Foldable f) => f Integer -> [[Integer]]
foldl' = Foldl.fold (toFoldl (myMealy' 10))

-- escaping skolems
data Mealy'' a b x = Mealy'' x (a -> x -> (x,b))

myMealy'' :: (Ord a, Num a) => a -> Mealy'' a [a] a
myMealy'' trigger = Mealy'' 0 (\a x -> swap (myStep trigger a x))

toPipeWithSkolem :: (Monad m, Ord a, Num a) => Mealy'' a [a] a-> Pipe a [a] m ()
toPipeWithSkolem (Mealy'' begin step) =
  go begin
  where
    go x = do
      a <- await
      let (x',b) = step a x
      yield b
      go x'

skolem :: (Ord a, Num a, Foldable f) => f a -> [[a]]
skolem ns = Pipes.toList (each ns >-> toPipeWithSkolem (myMealy'' 10)) 

-- fold

-- myStep without an output tape
myStep' :: (Ord t, Num t) => t -> ([[t]],t) -> t -> ([[t]],t)
myStep' trigger (out,acc) a =
  let acc' = acc + a in
  if acc' >= trigger
  then ([acc']:out,0)
  else ([]:out,acc')

fold :: (Num a, Ord a, Foldable f) => f a -> [[a]]
fold ns = fst $ F.foldl' (myStep' 10) ([],0) ns

--criterion helpers
data Speed =
    Speed
    { _speedMutator :: Double
    , _speedGc      :: Double
    } deriving (Show)

speed' :: (Integral t) => Control.DeepSeq.NFData b => t -> (a -> b) -> a -> IO Speed
speed' nSamples f a = do
    (m,_) <- Criterion.Measurement.measure (nf f a) (fromIntegral nSamples)
    return $ (\x -> Speed (measMutatorCpuSeconds x) (measGcCpuSeconds x)) m

render' :: String -> Int -> Speed -> Text
render' label n speed =
    sformat
    (string %"\t"% expt 2 %"\t"% expt 2 %"\t"% expt 2 %"\t"% expt 2 %"\n")
    label
    n
    (_speedMutator speed)
    (_speedGc speed)
    ((_speedGc speed + _speedMutator speed) / fromIntegral n)

-- speed test
main :: IO ()
main = do
  (ns':n':_) <- getArgs
  let ns = read ns'
      n = read n'
      t = ns*n
      str = replicate n 1
  Text.putStrLn "func\t\t\tn\tmutat\tgc\tspeed"
  Text.putStr =<< render' "machines        " t <$> speed' ns mealy' str
  Text.putStr =<< render' "pipes           " t <$> speed' ns pipe   str
  Text.putStr =<< render' "pipe - state    " t <$> speed' ns pipe'' str
  Text.putStr =<< render' "pipe - bad state" t <$> speed' ns pipe'  str
  Text.putStr =<< render' "foldl           " t <$> speed' ns foldl' str
  Text.putStr =<< render' "pipe & skolems  " t <$> speed' ns skolem str
  Text.putStr =<< render' "just a fold     " t <$> speed' ns fold str



-- machines vs pipes
producerToSource :: Monad m => Producer b m r -> M.PlanT k b m r
producerToSource p =
  runEffect $
  hoist lift p >->
  forever (do
      a <- await
      lift $ M.yield a)

source :: (Monad m) => M.MachineT m k Integer
source = M.construct (producerToSource (each [1..100])) 

summary :: (Monad m) => M.MachineT m k [Integer]
summary = M.cap (M.auto $ myMealy 10) source

-- waits until machine runs completely
badPrint :: Int -> IO ()
badPrint n = print =<< (take n <$> M.runT summary)

prodTee :: (Monad m) => M.MachineT (Producer a m) (M.Is a) ()
prodTee = void $ M.repeatedly $ do
  a <- M.await
  lift . yield $ a
  M.yield ()

fromProcess :: (Monad m) => Producer a m ()
fromProcess = M.runT_ prodTee

t1 :: IO ()
t1 = void $ runEffect $ void (M.runT (summary M.~> prodTee)) >-> Pipes.take 10 >-> Pipes.print


-- machines versus pipes
printMachine :: (Show a) => M.MachineT IO (M.Is [a]) ()
printMachine = M.repeatedly $ M.await >>= lift . print >> M.yield ()

machinePrint :: (Num a, Ord a, Show a) => [a] -> IO ()
machinePrint ns = M.runT_ $ M.supply ns (M.auto $ myMealy 10) M.~> printMachine

printPipe :: (Show a) => Consumer a IO ()
printPipe = forever $ await >>= lift . print

pipesPrint :: (Ord a, Num a, Show a, Foldable f) => f a -> IO ()
pipesPrint ns = runEffect $ each ns >-> toPipe (myMealy' 10) >-> printPipe 


