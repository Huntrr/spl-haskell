module PrettyPrinter where
  
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

class PP a where
  pp :: a -> Doc


render :: PP a => a -> String
render = PP.render . pp

instance PP Program where
  pp _ = undefined
