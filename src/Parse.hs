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

import Data.Char (digitToInt)

import Exp

parseExp :: String -> Maybe Exp
parseExp = fmap Exp . hush . parse (expr <* eof) "expression"

L.TokenParser {
  
  L.parens         = parens,
  L.reservedOp     = reservedOp,
  L.reserved       = reserved,
  L.whiteSpace     = whiteSpace,
  L.naturalOrFloat = naturalOrFloat,
  L.lexeme         = lexeme,
  L.decimal        = decimal

  } =
  
  L.makeTokenParser emptyDef { L.identLetter     = alphaNum
                             , L.identStart      = letter
                             , L.reservedNames   = words "true false"
                             , L.reservedOpNames = words "& | ! - + * > < >= <= == ~" }

-- | This is a direct copy of Parsec's 'Text.Parsec.Token.stringLiteral' modified
-- to parse single quotes instead of double.
stringLiteral   = lexeme (
                  do{ str <- between (char '\'')
                                     (char '\'' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter    = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringEscape    = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }
escapeEmpty     = char '&'
escapeGap       = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

charNum         = do{ code <- decimal
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })

escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

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

table =  [ [ prefix "-"  (Monop OpNegate . Exp), prefix "+" id ]
         , [ binary "*"  (Binop OpMult `on` Exp)    AssocLeft ]
         , [ binary "+"  (Binop OpPlus `on` Exp)    AssocLeft,
             binary "-"  (Binop OpMinus `on` Exp)   AssocLeft ]
         , [ binary "<"  (Binop OpLT `on` Exp)      AssocLeft,
             binary ">"  (Binop OpGT `on` Exp)      AssocLeft,
             binary "<=" (Binop OpLTE `on` Exp)     AssocLeft,
             binary ">=" (Binop OpGTE `on` Exp)     AssocLeft,
             binary "==" (Binop OpApprox `on` Exp)  AssocLeft,
             binary "~"  (Binop OpApprox `on` Exp)  AssocLeft ]
         , [ binary "&"  (Binop OpAnd `on` Exp)     AssocLeft,
             binary "|"  (Binop OpOr `on` Exp)      AssocLeft ] ]


val   =      Val (Bool True)  <$  reserved "true"      
         <|> Val (Bool False) <$  reserved "false"
         <|> Val . String     <$> stringLiteral
         <|> Val . Num        <$> allFloat
         <?> "value"
  where allFloat = f <$> naturalOrFloat
        f (Left  int)   = fromIntegral int
        f (Right float) = float
