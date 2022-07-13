module Print
  ( printEm
  , printEms
  ) where


import           Data.List                      ( sortOn )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Types


printEm :: EggMove -> IO ()
printEm em = do
  TIO.putStrLn (move em)
  let spaces = T.replicate 4 " "
  TIO.putStrLn
    (  spaces
    <> "Level up: "
    <> (T.intercalate ", " . map unPokemon $ levelParents em)
    )
  TIO.putStrLn
    (  spaces
    <> "Breed: "
    <> (T.intercalate ", " . map unPokemon $ breedParents em)
    )


printEms :: [EggMove] -> IO ()
printEms ems = mapM_ printEm $ sortOn move ems
