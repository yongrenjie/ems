module Types
  ( EggMove(..)
  , Pokemon(..)
  , unPokemon
  ) where


import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Text.HTML.TagSoup              ( Tag )


data EggMove = EggMove
  { move         :: Text
  ,            -- the name of the move
    levelParents :: [Pokemon]
  ,  -- parents which learn through levelling up
    breedParents :: [Pokemon]
  ,  -- parents which learn through breeding
    tags         :: [Tag ByteString] -- debug
  }
  deriving (Eq, Show)


newtype Pokemon = Pokemon Text deriving (Eq, Ord, Show)

unPokemon :: Pokemon -> Text
unPokemon (Pokemon t) = t


