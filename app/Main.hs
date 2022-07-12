{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Req
import Text.HTML.TagSoup
import Options.Applicative

getSerebiiHTML :: Text -> IO B.ByteString
getSerebiiHTML pkmn = runReq defaultHttpConfig $ do
  r <- req
        GET
        (https "serebii.net" /: "pokedex-swsh" /: pkmn /: "egg.shtml")
        NoReqBody
        bsResponse
        mempty
  pure $ responseBody r

main :: IO ()
main = do
  opts <- execParser options
  getSerebiiHTML (name opts) >>= B.putStrLn


data Options = Options {
  name :: Text
}


options :: ParserInfo Options
options = info (parseArgv <**> helper)
               (fullDesc <> progDesc "Get egg moves for a Pokemon")

parseArgv :: Parser Options
parseArgv = Options <$> argument str (metavar "POKEMON")
