
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit

import Control.Monad

import Exp
import Parse

eval :: String -> Maybe Val
eval = parseExp >=> interpret

main :: IO ()
main = defaultMain [
  testGroup "Total unit" [

     testCase "2/2 ==> fail" (eval "2/2" @?= Nothing),

     testCase "'string' exists"
     (eval "'string'" @?= Just (String "string")),
     
     testCase "'ab' == 'ab'"
     (eval "'ab' == 'ab'" @?= Just (Bool True)),
     
     testCase "'a' + 'b' == 'ab'"
     (eval "'a' + 'b' == 'ab'" @?= Just (Bool True))
     
     ],
  
  testGroup "Expression interpretation" [

     testCase "Booleans are preserved"
     (interpret (Exp (Val (Bool True))) @?= Just (Bool True)),

     testCase "Doubles are preserved"
     (interpret (Exp (Val (Num 0.0))) @?= Just (Num 0.0)),

     testGroup "Operations..." [
       testGroup "Bool to Bool" [
          testProperty "and" $ \b1 b2 ->
            Just (Bool (b1 && b2))
            == interpret (Exp (Binop OpAnd
                               (Exp (Val (Bool b1)))
                               (Exp (Val (Bool b2))))),
          testProperty "or" $ \b1 b2 ->
            Just (Bool (b1 || b2))
            == interpret (Exp (Binop OpOr
                               (Exp (Val (Bool b1)))
                               (Exp (Val (Bool b2))))),
          testProperty "not" $ \b ->
            Just (Bool (not b))
            == interpret (Exp (Monop OpNot
                               (Exp (Val (Bool b)))))
          ],

       testGroup "Int to Bool" $ flip map [
         ("gt", (>), OpGT),
         ("lt", (<), OpLT),
         ("gte", (>=), OpGTE),
         ("lte", (<=), OpLTE),
         ("approx", \a b -> abs (a - b) < eps, OpApprox)
         ] $ \(name, op, opCon) ->
           testProperty name $ \n1 n2 ->
             Just (Bool (n1 `op` n2))
             == interpret (Exp (Binop opCon
                                (Exp (Val (Num n1)))
                                (Exp (Val (Num n2))))),

       testGroup "Bool to Bool" $ flip map [
         ("and", (&&), OpAnd),
         ("or", (||), OpOr)
         ] $ \(name, op, opCon) ->
           testProperty name $ \n1 n2 ->
             Just (Bool (n1 `op` n2))
             == interpret (Exp (Binop opCon
                                (Exp (Val (Bool n1)))
                                (Exp (Val (Bool n2)))))

             
       ]
     ]
  ]
