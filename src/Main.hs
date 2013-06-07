-- |
-- Module      : Main
-- Copyright   : 2013 (c) Joseph Abrahamson, Reify Health
-- License     : AllRightsReserved
-- 
-- Maintainer  : joseph@reifyhealth.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Conditional expression parser for TDL2
module Main where

import Exp
import Parse
import Control.Monad

printer :: Maybe Result -> String
printer Nothing = "fail"
printer (Just x) = "ok " ++ printer' x where
  printer' (RBool True)  = "bool true"
  printer' (RBool False) = "bool false"
  printer' (RNum  f)     = "num " ++ show f

main = do
  l <- getLine
  putStrLn $ printer $ interpret <=< parseExp $ l