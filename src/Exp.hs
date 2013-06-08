-- |
-- Module      : Exp
-- Copyright   : 2013 (c) Joseph Abrahamson, Reify Health
-- License     : AllRightsReserved
-- 
-- Maintainer  : joseph@reifyhealth.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Conditional expressions
module Exp where

import Control.Monad
import Control.Applicative

-- | The expression functor

data Val = Num Double
         | Bool Bool
         | String String
         deriving (Eq, Show)

data Binop  = OpLT | OpGT | OpLTE | OpGTE | OpApprox
            | OpAnd | OpOr
            | OpPlus | OpMinus | OpMult
            deriving (Eq, Show)
data Monop  = OpNot
            | OpNegate
            deriving (Eq, Show)

data ExpF a = Val Val
            | Binop Binop a a
            | Monop Monop a
            deriving (Eq, Show)

data Type   = TyNum | TyBool | TyString deriving (Eq, Show)

newtype Exp  = Exp (ExpF Exp) deriving (Eq, Show)
newtype TExp = TExp (ExpF TExp, Type) deriving (Eq, Show)

eps = 0.01

doMonOp :: Monop -> Val -> Maybe Val
doMonOp OpNot    (Bool b) = Just $ Bool $ not b
doMonOp OpNegate (Num  d) = Just $ Num  $ negate d
doMonOp _        _         = Nothing

doBinOp :: Binop -> Val -> Val -> Maybe Val
  -- Num -> Num -> Bool
doBinOp OpLT     (Num a) (Num b)   = Just $ Bool $ a < b
doBinOp OpGT     (Num a) (Num b)   = Just $ Bool $ a > b
doBinOp OpLTE    (Num a) (Num b)   = Just $ Bool $ a <= b
doBinOp OpGTE    (Num a) (Num b)   = Just $ Bool $ a >= b
doBinOp OpApprox (Num a) (Num b)   = Just $ Bool $ abs (a - b) < eps
  -- String -> String -> Bool
doBinOp OpApprox (String a) (String b) =
  Just $ Bool $ a == b
  -- String -> String -> String
doBinOp OpPlus   (String a) (String b) =
  Just $ String $ a ++ b
  -- Num -> Num -> Num
doBinOp OpPlus   (Num a) (Num b)   = Just $ Num $ a + b
doBinOp OpMinus  (Num a) (Num b)   = Just $ Num $ a - b
doBinOp OpMult   (Num a) (Num b)   = Just $ Num $ a * b
  -- Bool -> Bool -> Bool
doBinOp OpAnd    (Bool a) (Bool b) = Just $ Bool $ a && b
doBinOp OpOr     (Bool a) (Bool b) = Just $ Bool $ a || b
doBinOp _        _         _         = Nothing

interpret :: Exp -> Maybe Val
interpret (Exp (Val (Num d)))        = Just (Num d)
interpret (Exp (Val (Bool d)))       = Just (Bool d)
interpret (Exp (Val (String d)))     = Just (String d)
interpret (Exp (Monop op a))   = interpret a >>= doMonOp op
interpret (Exp (Binop op a b)) = do
  ra <- interpret a
  rb <- interpret b
  doBinOp op ra rb