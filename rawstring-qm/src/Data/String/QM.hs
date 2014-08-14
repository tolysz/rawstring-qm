{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, 
  UndecidableInstances, OverlappingInstances, MultiParamTypeClasses,
  IncoherentInstances
  #-}


module Data.String.QM
 (qq, qm)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Prelude ((.), ($), fail, map, return, foldl,foldl1, foldr)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import GHC.Exts (IsString(..))
import Data.Monoid (Monoid(..))
import Data.ByteString.Char8 as Strict (ByteString, unpack)
import Data.ByteString.Lazy.Char8 as Lazy (ByteString, unpack)
import Data.Text as T (Text, unpack)
import Data.Text.Lazy as LazyT(Text, unpack)
import Data.Char (isAlpha, isAlphaNum)
import Prelude
import Data.Maybe

data StringPart = Literal String | AntiQuote String | Lookup String deriving Show


qq :: QuasiQuoter
qq = QuasiQuoter 
    { quoteExp  = return . LitE . StringL
    -- , quotePat  = return . ListP . map (LitP . CharL)
    , quotePat  = return . bla
    , quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)"
    , quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
}

-- lets have

bla [c] = LitP (CharL c)
bla (c:cs) = InfixP (LitP (CharL c)) '(:) (bla cs)


unQM a []          = [Literal (reverse a)]
unQM a ('\\':x:xs) = unQM (x:a) xs
unQM a ('\\':[])   = unQM ('\\':a) []
unQM a ('}':xs)    = Lookup (reverse a) : parseQM [] xs
unQM a (x:xs)      = unQM (x:a) xs

parseQM a []           = [Literal (reverse a)]
parseQM a ('\\':x:xs)  = parseQM (x:a) xs
parseQM a ('\\':[])    = parseQM ('\\':a) []
parseQM a ('$':x:xs) | x == '_' || isAlpha x =
    Literal (reverse a) : AntiQuote (x:pre) : parseQM [] post
    where
    (pre, post) = span isIdent xs
parseQM a ('{':xs)     = Literal (reverse a) : unQM [] xs
parseQM a (x:xs)       = parseQM (x:a) xs


isIdent '_'  = True
isIdent '\'' = True
isIdent x    = isAlphaNum x

makeExpr [] = ls ""
makeExpr ((Literal a):xs)   = TH.appE [| (++) a |] 
                            $ makeExpr xs
makeExpr ((AntiQuote a):xs) = TH.appE [| (++) $(reifyM a) |]
                            $ makeExpr xs

ls = return . TH.LitE . TH.StringL

makeExprF1 a = 
  if (hasLookup a)
   then
     do
      l <- TH.newName "lookup" -- string -> value
      x <- makeExprF l a
      return $ TH.LamE [TH.VarP l ] x
   else
     makeExpr a

makeExprF l [] = ls ""
makeExprF l ((Literal a):xs)   = TH.appE [| (++) a  |] 
                                    $ makeExprF l xs
makeExprF l ((AntiQuote a):xs) = TH.appE [| (++) $(reifyM a) |] 
                              $ makeExprF l xs
makeExprF l ((Lookup a):xs) = TH.appE [| (++) ((fromMaybe $(reifyM a) $( return $ TH.AppE (TH.VarE l) (TH.LitE (TH.StringL a)) ))  ) |] 
                              $ makeExprF l xs

hasLookup []               = False
hasLookup ((Lookup _ ):as) = True
hasLookup  (_:as)          = hasLookup as


-- | QuasiQuoter for interpolating '$var' and '{expr}' into a string literal. The pattern portion is undefined.
qm :: QuasiQuoter
qm = QuasiQuoter (makeExprF1 . parseQM [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

reifyM s = 
    case parseExp s of
        Left s  -> TH.reportWarning s >> ls ""
        Right e ->  return e

