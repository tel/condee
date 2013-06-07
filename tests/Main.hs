
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit

import Exp

main :: IO ()
main = defaultMain [
  testGroup "Expression interpretation" [

     testCase "Booleans are preserved"
     (interpret (Exp (Bool True)) @?= Just (RBool True)),

     testCase "Doubles are preserved"
     (interpret (Exp (Num 0.0)) @?= Just (RNum 0.0)),

     testGroup "Operations..." [
       testGroup "Bool to Bool" [
          testProperty "and" $ \b1 b2 ->
            Just (RBool (b1 && b2))
            == interpret (Exp (Binop OpAnd
                               (Exp (Bool b1))
                               (Exp (Bool b2)))),
          testProperty "or" $ \b1 b2 ->
            Just (RBool (b1 || b2))
            == interpret (Exp (Binop OpOr
                               (Exp (Bool b1))
                               (Exp (Bool b2)))),
          testProperty "not" $ \b ->
            Just (RBool (not b))
            == interpret (Exp (Monop OpNot
                               (Exp (Bool b))))
          ],

       testGroup "Int to Bool" $ flip map [
         ("gt", (>), OpGT),
         ("lt", (<), OpLT),
         ("gte", (>=), OpGTE),
         ("lte", (<=), OpLTE),
         ("approx", \a b -> abs (a - b) < eps, OpApprox)
         ] $ \(name, op, opCon) ->
           testProperty name $ \n1 n2 ->
             Just (RBool (n1 `op` n2))
             == interpret (Exp (Binop opCon
                                (Exp (Num n1))
                                (Exp (Num n2)))),

       testGroup "Bool to Bool" $ flip map [
         ("and", (&&), OpAnd),
         ("or", (||), OpOr)
         ] $ \(name, op, opCon) ->
           testProperty name $ \n1 n2 ->
             Just (RBool (n1 `op` n2))
             == interpret (Exp (Binop opCon
                                (Exp (Bool n1))
                                (Exp (Bool n2))))

             
       ]
     ]
  ]