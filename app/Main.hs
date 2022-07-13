{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as TIO
import           Network.HTTP.Req
import           Options.Applicative
import Data.List (sort)

import           GetEggMoves


getSerebiiHTML :: Text -> IO ByteString
getSerebiiHTML pkmn = runReq defaultHttpConfig $ do
  r <- req GET
           (https "serebii.net" /: "pokedex-swsh" /: pkmn /: "egg.shtml")
           NoReqBody
           bsResponse
           mempty
  pure $ responseBody r


main :: IO ()
main = do
  opts <- execParser options
  html <- getSerebiiHTML (name opts)
  let ems = sort $ getEggMoves html
  mapM_ TIO.putStrLn ems


-- Command-line option parsing

data Options = Options
  { name :: Text
  }

options :: ParserInfo Options
options = info (parseArgv <**> helper)
               (fullDesc <> progDesc "Get egg moves for a Pokemon")

parseArgv :: Parser Options
parseArgv = Options <$> argument str (metavar "POKEMON")
