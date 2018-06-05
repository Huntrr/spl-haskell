{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module ExceptionPrinter (exceptionPretty) where

import AST
import Data.Set (Set, toList)
import Data.Map (Map, foldrWithKey)

import Text.PrettyPrint
import Text.Megaparsec
import Text.Numeral.Roman (toRoman)
import qualified Data.List as List

exceptionPretty :: Exception -> String
exceptionPretty = ("\nException:\n" ++) . render . nest 2 . pp

class PP a where
  pp :: a -> Doc

instance PP Exception where
  pp (DivideByZero a)   =
    pp a $$ text "Attempted division by zero"

  pp (UnrealAnswer a)   = 
    pp a $$ text "Expression evaluated to an unreal value"

  pp (EmptyStack a s)   =
    pp a $$ pp s $$ text "Tried to pop from an empty stack"

  pp (AmbiguousYou a s) =
    pp a $$ (text "Possible choices:" <+> brackets (pp s)) $$
      (text "Ambiguous declaration")

  pp (NotOnStage c a s) =
    pp a $$ (text "On stage: " <+> brackets (pp s)) $$
      text (c ++ " is not on stage")

  pp (AlreadyOnStage c a s) =
    pp a $$ (text "On stage: " <+> brackets (pp s)) $$
      text (c ++ " is already on stage")

  pp (UndefinedCondition a s)   =
    pp a $$ pp s $$ text "Branch with condition still undefined"

  pp (InvalidAct l) =
    text $ "Can't jump to invalid act, Act " ++ toRoman l

  pp (InvalidScene l) =
    text $ "Can't jump to invalid scene, Scene " ++ toRoman l


instance PP (Set String) where
  pp = text . List.intercalate ", " . toList

{-data Store = Store { variables     :: Map CName (Value, [Value])-}
                   {-, onStage       :: Set CName-}
                   {-, condition     :: Maybe Bool-}
                   {-, output        :: Maybe String-}
                   {-, awaitingInput :: Maybe (CName, InputType)-}
                   {-, act           :: Label-}
                   {-, scene         :: Label-}
                   {-} deriving (Eq, Show)-}

instance (PP a) => PP (Maybe a) where
  pp Nothing = text "N/A"
  pp (Just a) = pp a

instance PP String where pp = text . show
instance PP Bool where pp = text . show
instance PP (CName, InputType) where
  pp (c, t) = parens (case t of
     InChar -> text "Char"
     InInt  -> text "Int") <> text c

instance PP Store where
  pp (Store vars stage cond output input act scene) =
    text ("Act " ++ toRoman act ++ ", Scene " ++ toRoman scene) <+>
      brackets (text "Condition:" <+> pp cond <> comma <+>
        text "Output:" <+> pp output <> comma <+>
          text "Input:" <+> pp input <> comma) $$
            (text "Variables:" $$ pp vars) $$
              (nest 30 $ text "On stage:" $$ pp stage)

instance PP (Map CName (Value, [Value])) where
  pp = foldrWithKey f empty where
    f k v = ((quotes (text k) <+> text "\t=>" <+> g v) $$)
    g (v, s) = text (show v) <+> text "\t|" <+> text (show s)

instance PP Annotation where
  pp (Annotation line pos) = pp pos <+> doubleQuotes (text line)

instance PP Text.Megaparsec.SourcePos where
  pp = text . sourcePosPretty
