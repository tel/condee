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

import System.Environment
import Control.Monad
import Control.Applicative
import Data.List

import Exp
import Parse


printer :: Maybe Result -> String
printer Nothing = "fail"
printer (Just x) = "ok " ++ printer' x where
  printer' (RBool True)  = "bool true"
  printer' (RBool False) = "bool false"
  printer' (RNum  f)     = "num " ++ show f

main = do
  args <- getArgs
  line <- if null args then getLine else return $ intercalate " " args
  putStrLn . printer $ interpret <=< parseExp $ line
  