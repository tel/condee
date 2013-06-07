-- |
-- Module      : Parse
-- Copyright   : 2013 (c) Joseph Abrahamson, Reify Health
-- License     : AllRightsReserved
-- 
-- Maintainer  : joseph@reifyhealth.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Parsec parser for Exp

module Parse (parseExp) where

import Data.Function
import Control.Monad
import Control.Applicative
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as L
import Text.Parsec.Language (emptyDef)

import Exp

parseExp :: String -> Maybe Exp
parseExp = fmap Exp . hush . parse (expr <* eof) "expression"

L.TokenParser {
  L.parens     = parens,
  L.reservedOp = reservedOp,
  L.reserved   = reserved,
  L.whiteSpace = whiteSpace,
  L.naturalOrFloat = naturalOrFloat
  } = L.makeTokenParser
      emptyDef { L.identLetter = alphaNum
               , L.identStart = letter
               , L.reservedNames = words "true false",
                 L.reservedOpNames = words "& | ! - + * > < >= <= == ~"
               }

hush :: Either s a -> Maybe a
hush (Left s) = Nothing
hush (Right t) = Just t

binary  name fun assoc = Infix (fun <$ reservedOp name) assoc
prefix  name fun       = Prefix (fun <$ reservedOp name)
postfix name fun       = Postfix (fun <$ reservedOp name)

term  =      parens expr
         <|> val
         <?> "simple expression"

expr  =  buildExpressionParser table term <?> "expression"

table =  [ [ prefix "-" (Monop OpNegate . Exp), prefix "+" id ]
         , [ binary "*" (Binop OpMult `on` Exp) AssocLeft ]
         , [ binary "+" (Binop OpPlus `on` Exp) AssocLeft,
             binary "-" (Binop OpMinus `on` Exp) AssocLeft ]
         , [ binary "<" (Binop OpLT `on` Exp) AssocLeft,
             binary ">" (Binop OpGT `on` Exp) AssocLeft,
             binary "<=" (Binop OpLTE `on` Exp) AssocLeft,
             binary ">=" (Binop OpGTE `on` Exp) AssocLeft,
             binary "==" (Binop OpApprox `on` Exp) AssocLeft,
             binary "~" (Binop OpApprox `on` Exp) AssocLeft ]
         , [ binary "&" (Binop OpAnd `on` Exp) AssocLeft,
             binary "|" (Binop OpOr `on` Exp) AssocLeft ] ]


val   =      Bool True  <$  reserved "true"      
         <|> Bool False <$  reserved "false"     
         <|> Num        <$> allFloat
         <?> "value"
  where allFloat = f <$> naturalOrFloat
        f (Left  int)   = fromIntegral int
        f (Right float) = float


-- monop =     OpNot    <$ string "not"
--         <|> OpNegate <$ string "neg"

-- binop =     OpAnd    <$ string "&&"
--         <|> OpOr     <$ string "||"
--         <|> OpLTE    <$ string "<="
--         <|> OpGTE    <$ string ">="
--         <|> OpLT     <$ string "<"
--         <|> OpGT     <$ string ">"
--         <|> OpApprox <$ string "="
--         <|> OpApprox <$ string "~"
--         <|> OpPlus   <$ string "+"
--         <|> OpMinus  <$ string "-"
--         <|> OpMult   <$ string "*"

-- monopExp = Monop <$> monop <*> expr
-- binopExp = Binop <$> expr <*> binop <*> expr