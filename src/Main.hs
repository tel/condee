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
import System.Exit
import Control.Monad
import Control.Applicative
import Data.List

import Exp
import Parse


printer :: Val -> String
printer (Bool   True)  = "bool true"
printer (Bool   False) = "bool false"
printer (Num    f)     = "num " ++ show f
printer (String s)     = "string " ++ show s

die :: String -> IO ()
die s = putStrLn ("fail: " ++ s) >> exitWith (ExitFailure (-1))

main :: IO ()
main = do
  args <- getArgs
  line <- if null args then getLine else return $ intercalate " " args

  case parseExp line of
    Nothing  -> die "parse error"
    Just ast -> case interpret ast of
      Nothing -> die "type error"
      Just res -> putStrLn (printer res)