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

data Binop  = OpLT | OpGT | OpLTE | OpGTE | OpApprox
            | OpAnd | OpOr
            | OpPlus | OpMinus | OpMult
            deriving (Eq, Show)
data Monop  = OpNot
            | OpNegate
            deriving (Eq, Show)
data Type   = TyNum | TyBool deriving (Eq, Show)
data ExpF a = Num Double
            | Bool Bool
            | Binop Binop a a
            | Monop Monop a
            deriving (Eq, Show)
newtype Exp  = Exp (ExpF Exp) deriving (Eq, Show)
newtype TExp = TExp (ExpF TExp, Type) deriving (Eq, Show)

data Result = RBool Bool | RNum Double deriving (Eq, Show)

eps = 0.01

doMonOp :: Monop -> Result -> Maybe Result
doMonOp OpNot    (RBool b) = Just $ RBool $ not b
doMonOp OpNegate (RNum  d) = Just $ RNum $ negate d
doMonOp _        _         = Nothing

doBinOp :: Binop -> Result -> Result -> Maybe Result
  -- Num -> Num -> Bool
doBinOp OpLT     (RNum a) (RNum b)   = Just $ RBool $ a < b
doBinOp OpGT     (RNum a) (RNum b)   = Just $ RBool $ a > b
doBinOp OpLTE    (RNum a) (RNum b)   = Just $ RBool $ a <= b
doBinOp OpGTE    (RNum a) (RNum b)   = Just $ RBool $ a >= b
doBinOp OpApprox (RNum a) (RNum b)   = Just $ RBool $ abs (a - b) < eps
  -- Num -> Num -> Num
doBinOp OpPlus   (RNum a) (RNum b)   = Just $ RNum $ a + b
doBinOp OpMinus  (RNum a) (RNum b)   = Just $ RNum $ a - b
doBinOp OpMult   (RNum a) (RNum b)   = Just $ RNum $ a * b
  -- Bool -> Bool -> Bool
doBinOp OpAnd    (RBool a) (RBool b) = Just $ RBool $ a && b
doBinOp OpOr     (RBool a) (RBool b) = Just $ RBool $ a || b
doBinOp _        _         _         = Nothing

interpret :: Exp -> Maybe Result
interpret (Exp (Num d))  = Just (RNum d)
interpret (Exp (Bool d)) = Just (RBool d)
interpret (Exp (Monop op a)) = interpret a >>= doMonOp op
interpret (Exp (Binop op a b)) = do
  ra <- interpret a
  rb <- interpret b
  doBinOp op ra rb