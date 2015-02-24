module ExampleFoldl where

import qualified Control.Foldl as Foldl
import qualified Pipes.Prelude as Pipes
import           Pipes
import           Control.Monad

-- example 1
stream1 :: (Monad m) => Producer Int m ()
stream1 = Pipes.each [4, 5, 2, 1, 8, 0, 4, 3]

foldlSummary :: Int -> Foldl.Fold Int [Int]
foldlSummary n = Foldl.Fold step begin done
  where
       begin = (0::Int,[])
       step x a = let acc = fst x + a
                  in if acc>=n
                     then (0,snd x++ [acc])
                     else (acc,snd x)                    
       done (acc, res) = res ++ [acc]

showStream :: Foldl.Fold Int [Int] -> Producer Int IO () -> IO ()
showStream f p =
  runEffect $
  p >-> Pipes.chain (\x -> putStrLn $ "orig: " ++ show x) >->
  Foldl.purely Pipes.scan f >->
  Pipes.chain (\x -> putStrLn $ "fold: " ++ show x) >->
  forever await

main :: IO ()
main = showStream (foldlSummary 10) stream1
