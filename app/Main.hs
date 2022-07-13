{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import qualified Data.ByteString               as B
import           Data.ByteString                ( ByteString )
import           Data.List                      ( sortOn )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as TIO
import           GetEggMoves
import           Network.HTTP.Req
import           Options
import           Control.Exception              ( handle )
import           System.IO                      ( stderr, hPutStrLn )


-- too lazy for ExceptT
getSerebiiHTML :: Game -> Text -> IO (Either Text ByteString)
getSerebiiHTML game pkmn =  do
  let serebiiUrl = https "serebii.net" /: "pokedex-swsh" /: pkmn /: "egg.shtml"
  let err = \(e :: HttpException) -> pure (Left $ "could not access URL: " <> renderUrl serebiiUrl)
  handle err $ do
    r <- runReq defaultHttpConfig (req GET serebiiUrl NoReqBody bsResponse mempty)
    pure $ Right (responseBody r)


-- IO BS >>= BS -> Eitehr Text BS ===  IO (Either Text BS)

-- runReq defaultHttpConfig :: Req BS -> IO BS
-- catch :: IO a -> (e -> IO a) -> IO a

main :: IO ()
main = do
  opts <- getOptions
  eitherHtml <- getSerebiiHTML (game opts) (pokemon opts)
  case eitherHtml of
       Left error -> TIO.hPutStrLn stderr error
       Right html -> do
         let ems = sortOn move $ getEm (game opts) html
         mapM_ printEm ems
