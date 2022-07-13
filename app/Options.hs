module Options
  ( Options(..)
  , Game(..)
  , getOptions
  ) where

import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import           Options.Applicative

data Game = SWSH | BDSP deriving (Eq, Ord, Show)

gameReader :: ReadM Game
gameReader = maybeReader f
 where
  f s = case map toLower s of
    "swsh" -> Just SWSH
    "bdsp" -> Just BDSP
    _      -> Nothing


data Options = Options
  { pokemon :: Text
  , game :: Game
  }


options :: ParserInfo Options
options = info (parseOptions <**> helper)
               (fullDesc <> progDesc "Get SwSh or BDSP egg moves for a Pokemon. Note that this doesn't differentiate between regional forms.")


parseOptions :: Parser Options
parseOptions = 
  Options
    <$> argument str (metavar "POKEMON" <> help "The Pokemon")
    <*> argument
          gameReader
          (metavar "GAME" <> value SWSH <> help
            "Game to find EMs in [swsh/bdsp]"
          )


getOptions :: IO Options
getOptions = execParser options
