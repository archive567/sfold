{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.DocTest

main :: IO ()
main = do
    doctest ["src/Control/Foldl/Folds.hs"]
