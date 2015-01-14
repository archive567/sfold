module Main where

import Control.Category ((.))
import Prelude hiding ((.))
import           Control.Monad
import qualified Control.SFold as SFold
import qualified Control.SFold.Util as SFold
import           Pipes
import qualified Pipes.Prelude as Pipes

-- example 2
stream1 :: (Monad m) => Producer Int m ()
stream1 = Pipes.each [4, 5, 2, 1, 8, 0, 4, 3] 

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

-- showStream' (foldMultiples 10) stream1


-- variation: 10s
foldTens :: Int -> SFold.SFold Int Int
foldTens n = SFold.SFold to step begin release flush
  where
       to a      = (a,[])
       begin     = (0,[])
       step x x' = let acc = fst x + fst x'
                   in if acc>=n
                      then let y = acc `div` n
                           in (acc-y * n,snd x ++ snd x' ++ replicate y 1)
                      else (acc,snd x ++ snd x')                    
       release x = ((fst x,[]),snd x)
       flush x   = if fst x == 0
         then (0,snd x)
         else (0,snd x ++ replicate (fst x `div` n) 1)

-- showStream' (foldTens 10) stream1


-- showStream' (foldSummary 2 . foldMultiples 6 . foldTens 3) stream1


main :: IO ()
main = showStream' (foldSummary 2 . foldMultiples 6 . foldTens 3) stream1
