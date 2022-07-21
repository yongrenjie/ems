module Print
  ( printEms
  ) where


import           Data.List                      ( sortOn )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Prettyprinter
import           Prettyprinter.Render.Terminal  ( AnsiStyle(..)
                                                , bold
                                                , putDoc
                                                )
import           Prettyprinter.Util             ( putDocW
                                                , reflow
                                                )
import           Types


makeDoc :: EggMove -> Doc AnsiStyle
makeDoc em = vsep [moveD, indent 4 parentsD]
 where
  makeCommaSeparatedText :: [Pokemon] -> Text
  makeCommaSeparatedText = T.intercalate ", " . map unPokemon
  moveD                  = annotate bold $ pretty (move em)
  levelParentsD =
    "Level :" <+> indent 0 (reflow . makeCommaSeparatedText $ levelParents em)
  breedParentsD =
    "Breed :" <+> indent 0 (reflow . makeCommaSeparatedText $ breedParents em)
  parentsD = vsep [levelParentsD, breedParentsD, hardline]


printEms :: [EggMove] -> IO ()
printEms = putDoc . vsep . map makeDoc
