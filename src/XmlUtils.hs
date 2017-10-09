-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables  #-}
module XmlUtils where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Data.Time

q2Str :: QName -> Name
q2Str (N n) = n
q2Str (QN ns n) = n

attrStr n (Elem _ as _) =
    case lookup n as of
      Nothing -> fail ("expected attribute " ++ q2Str n)
      Just av -> return (attr2str av)

attrRead n e = do
    str <- attrStr n e
    case reads str of
      [(v,_)] -> return v
      _       -> fail $ "cannot parse attribute " ++ q2Str n ++ ": " ++ str

mkElemAC x as cs = CElem (Elem x as cs) ()

readText :: Read a => XMLParser a
readText = do
  t <- text
  case reads t of
    [(v,_)] -> return v
    _       -> fail $ "cannot parse " ++ t


instance XmlContent Bool where
  parseContents = do
    e@(Elem t _ _) <- element ["True", "False"]
    commit $ interior e $ case t of
      N "True"  -> return True
      N "False" -> return False

  toContents True  = [mkElemC "True"  []]
  toContents False = [mkElemC "False" []]

instance XmlContent Double where
  parseContents = inElement "Double" readText
  toContents t  = [mkElemC "Double" (toText (show t))]

instance HTypeable UTCTime where
  toHType _ = Defined "Time" [] []

instance XmlContent UTCTime where
  parseContents = inElement "Time" readText
  toContents t  = [mkElemC "Time" (toText (show t))]
