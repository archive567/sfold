module ExampleDeconstruct where

import Prelude hiding ((.))
import qualified Pipes.Prelude as Pipes
import Pipes hiding (X)
import qualified Control.SFold as SFold
import qualified Control.SFold.Util as SFold
import Control.Category ((.))
import Control.Arrow (second)
import Data.Monoid
import Control.Monad

-- example 2
stream1 :: (Monad m) => Producer Int m ()
stream1 = Pipes.each [4, 5, 10, 12, 0, 0, 42] 

foldSummary :: Int -> SFold.SFold Int Int
foldSummary n =
  SFold.SFold to step begin release flush
  where to a = (a,[])
        begin = (0,[])
        step x x' =
          let acc = fst x + fst x'
          in if acc >= n
                then (0,snd x ++
                        snd x' ++
                        [acc])
                else (acc,snd x ++ snd x')
        release x = ((fst x,[]),snd x)
        flush x =
          if fst x == 0
             then (0,snd x)
             else (0
                  ,snd x ++
                   [fst x])

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
       to a      = (a,[])
       begin     = (0,[])
       step x x' = let acc = fst x + fst x'
                   in if acc>=n
                      then let y = (acc `div` n) * n
                           in (acc-y,snd x ++ snd x' ++ [y])
                      else (acc,snd x ++ snd x')                    
       release x = ((fst x,[]),snd x)
       flush x   = if fst x == 0
         then (0,snd x)
         else (0,snd x ++ [fst x])

tMult :: [Int] -> IO ()
tMult xs = runEffect $ 
                      SFold.sfold (foldMultiples 10) (Pipes.each xs) >-> 
                      Pipes.print

-- variation: 10s
foldTens :: Int -> SFold.SFold Int Int
foldTens n = SFold.SFold to step begin release flush
  where
       to a      = (a,[])
       begin     = (0,[])
       step x x' = let acc = fst x + fst x'
                   in if acc>=n
                      then let y = acc `div` n
                           in (acc-y * n,snd x ++ snd x' ++ replicate y n)
                      else (acc,snd x ++ snd x')                    
       release x = ((fst x,[]),snd x)
       flush x   = if fst x == 0
         then (0,snd x)
         else (0,snd x ++ replicate (fst x `div` n) n)

tTens :: [Int] -> IO ()
tTens xs = runEffect $ 
                      SFold.sfold (foldTens 10) (Pipes.each xs) >-> 
                      Pipes.print


-- some deconstruction
t' :: [Int] -> IO ()
t' xs = runEffect $ 
                   SFold.sfold (foldMultiples 100 . foldSummary 10) (Pipes.each xs) >-> 
                   Pipes.print

t10 :: [Int] -> IO ()
t10 xs = runEffect $ 
                    SFold.sfold (foldSummary 10) (Pipes.each xs) >-> 
                    Pipes.print

t100 :: [Int] -> IO ()
t100 xs = runEffect $ 
                     SFold.sfold (foldSummary 100) (Pipes.each xs) >-> 
                     Pipes.print

main :: IO ()
main = t' [100]



data X = X (Int, [Int]) deriving (Show)

instance Monoid X where
  mempty = X (0,[])
  mappend (X x) (X x') = X (fst x + fst x', snd x ++ snd x')

to' :: Int -> X
to' a      = X (a,[])

begin' :: X
begin'     = X (0,[])

step' :: Int -> X -> X -> X
step' n (X x) (X x') = let acc = fst x + fst x'
                       in if acc>=n
                          then X (0,snd x ++ snd x' ++ [acc])
                          else X (acc,snd x ++ snd x')                    

release' :: X -> (X, [Int])
release' (X x) = (X (fst x,[]),snd x)

flush' :: X -> X
flush' (X x)   = if fst x == 0
  then X (0,snd x)
  else X (0,snd x ++ [fst x])

flushr' :: [Int] -> X
flushr' = foldr (step' 100 . to') (mempty::X)

to'' :: Int -> (X, X)
to'' = second flushr' . release' . flush' . to'

step'' :: (X, X) -> (X, X) -> (X, X)
step'' (x0,x1) (x0',x1') = (step' 10 x0 x0', step' 100 x1 x1')

begin'' :: (X, X)
begin'' = (mempty::X,mempty::X)
  
release'' :: (X, X) -> ((X, X), [Int])
release'' (x0,x1) =
  let (x0',out) = release' x0
      x1' = step' 100 x1 (flushr' out)
      (x1'',out') = release' x1' in
  ((x0',x1''),out')

flush'' :: (X, X) -> (X, X)
flush'' (x0,x1) =
  let x0' = flush' x0
      (x0'',out) = release' x0'
      x1' = step' 100 x1 (flushr' out)
      x1'' = flush' x1' in
  (x0'',x1'')
