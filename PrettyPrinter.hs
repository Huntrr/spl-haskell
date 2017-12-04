{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module PrettyPrinter where
  
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Test.QuickCheck (generate,elements,vectorOf)
import Control.Monad (liftM2,liftM3,foldM)
import WordLists as W
import Text.Numeral.Roman
import Text.Megaparsec (SourcePos)

import AST

class PP a where
  pp :: a -> IO Doc

newline :: Doc
newline = PP.char '\n'

dot :: Doc
dot = PP.char '.'

catSep :: [Doc] -> Doc -> Doc
catSep l sep = foldr (\x y -> x PP.<+> sep PP.<+> y) PP.empty l

{-- render :: PP a => a -> String
render = PP.render . pp --}

{-- Randomized parts of speech --}

exclamOrPeriod :: IO Doc
exclamOrPeriod = do
  c <- generate $ elements ['!', '.']
  return $ PP.char c

secondPersonDeclare :: IO Doc
secondPersonDeclare = do
  adj <- generate $ elements (W.negativeAdjectives ++ 
                              W.neutralAdjectives ++ 
                              W.positiveAdjectives)
  declr <- generate $ elements ["You", "You are", "You are as " ++ adj ++ " as"]
  return $ PP.text declr

goTo :: IO Doc
goTo = do
  pronoun <- generate $ elements ["Let us", "We shall", "We must"]
  verb <- generate $ elements ["return to", "proceed to"]
  return $ PP.text pronoun PP.<+> PP.text verb

generateConstant :: Int -> IO Doc
generateConstant n = do
  adjs <- generate $ vectorOf (floor $ logBase 2 (abs (fromIntegral n)) :: Int) 
                              (elements (W.negativeAdjectives ++ 
                                         W.neutralAdjectives ++
                                        W.positiveAdjectives))
  noun <- if n > 0 then generate $ elements W.positiveNouns else
                        generate $ elements W.negativeNouns
  return $ PP.hsep (PP.text <$> adjs) PP.<+> PP.text noun


{-- 
NOTE: I don't think we need the functions below
To pretty print, just call pp $ ____

splPretty :: Program -> IO Doc
splPretty = undefined

splChar :: String -> IO Doc
splChar = pp

docToString :: IO Doc -> IO String
docToString d = do
  doc <- d
  return $ PP.render doc

printString :: IO String -> IO ()
printString s = do
  str <- s
  print str --}

instance PP Program where
  pp _ = undefined

instance PP Character where
  pp (Character name desc) = liftM3 (\x y z -> x PP.<> y PP.<+> z) 
                                (pp name) (return PP.comma) (pp desc)

instance PP String where
  pp s = return $ PP.text s

instance PP Header where
  pp (Header t l) = liftM3 (\x y z -> x PP.<> y PP.$$ z)
                      (pp t) 
                      (return newline) 
                      (foldM (\d c -> liftM2 (PP.$$) (return d) (pp c)) 
                        PP.empty l)

commandWithCharList :: String -> [CName] -> IO Doc
commandWithCharList s l = (\x -> PP.lbrack PP.<> PP.text s PP.<+> x PP.<> PP.rbrack) <$> chars where
  chars = foldM (\d c -> liftM2 (\x y -> x PP.<+> PP.text "and" PP.<+> y) 
                                (return d) (pp c)) PP.empty l

instance PP Statement where
  pp (Enter l) = commandWithCharList "Enter" l
  pp (Exeunt l) = commandWithCharList "Exeunt" l
  pp (Exit name) = (\x -> PP.lbrack PP.<> PP.text "Exit" PP.<+> x PP.<> PP.rbrack) <$>
                   pp name
  pp (Line name sentence) = liftM3 (\x y z -> x PP.<> PP.colon PP.$$ PP.space PP.<> y PP.<> z) 
                                   (pp name) (pp sentence) exclamOrPeriod

instance PP Sentence where
  pp (IfSo s) = (\ x -> PP.text "If so," PP.<+> x) <$> pp s
  pp (IfNot s) = (\ x -> PP.text "If not," PP.<+> x) <$> pp s
  pp OutputNumber = return $ PP.text "Open your heart"
  pp OutputCharacter = return $ PP.text "Speak your mind"
  pp InputNumber = return $ PP.text "Listen to your heart"
  pp InputCharacter = return $ PP.text "Open your mind"
  pp (Declaration e) = liftM2 (PP.<+>) secondPersonDeclare (pp e)
  pp (Push r) = (\x -> PP.text "Remember" PP.<+> x) <$> pp r
  pp Pop = return $ PP.text "Recall your imminent death!" -- TODO: randomize this
  pp (GotoScene l) = liftM2 (\x y -> x PP.<+> PP.text "Scene" PP.<+> y) goTo (pp l)
  pp (GotoAct l) = liftM2 (\x y -> x PP.<+> PP.text "Act" PP.<+> y) goTo (pp l)
  pp (Conditional c) = pp c


instance PP Label where
  pp l = return $ toRoman l

instance PP Reference where
  pp You = do
            you <- generate $ elements (W.secondPerson ++ W.secondPersonReflexive)
            return $ PP.text you
  pp Me = do
            me <- generate $ elements (W.firstPerson ++ W.firstPersonReflexive)
            return $ PP.text me
  pp (They name) = pp name

instance PP Expression where
  pp (Constant 0) = return $ PP.text "nothing"
  pp (Constant v) = generateConstant v
  pp (Sum e1 e2) = liftM2 (\x y -> PP.text "the sum of" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                          (pp e1)
                          (pp e2)
  pp (Difference e1 e2) = liftM2 (\x y -> PP.text "the difference between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Product e1 e2) = liftM2 (\x y -> PP.text "the product of" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Quotient e1 e2) = liftM2 (\x y -> PP.text "the quotient between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Square e) = (\x -> PP.text "the square of" PP.<+> x) <$> pp e
  pp (Cube e) = (\x -> PP.text "the cube of" PP.<+> x) <$> pp e
  pp (SquareRoot e) = (\x -> PP.text "the square root of" PP.<+> x) <$> pp e
  pp (Twice e) = (\x -> PP.text "twice" PP.<+> x) <$> pp e
  pp (Mod e1 e2) = liftM2 (\x y -> PP.text "the remainder of the quotient between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Var r) = pp r

instance PP Comparison where
  pp (Comparison r e1 e2) = undefined

instance PP Relationship where
  pp l = undefined

instance PP Block where
  pp l = foldM (\d (s, Annotation a _) -> liftM2 (PP.$$) 
                                   (return d) 
                                   (if null a then pp s else return (PP.text a)))
                PP.empty (l :: [(Statement, Annotation)])

