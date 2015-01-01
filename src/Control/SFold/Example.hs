module Control.SFold.Example where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.SFold.Util
import Control.SFold.SBar
import Pipes.Prelude as Pipes
import Pipes

main :: IO ()
main = do
    let t1 = [(23,1),(2,2),(3,2),(5,6),(7,8),(34,10)]
    runEffect $ sfold (valueFold 10 . timeFold 3) (Pipes.each t1) >-> Pipes.print
