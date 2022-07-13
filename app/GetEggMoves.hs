{-# LANGUAGE OverloadedStrings #-}

module GetEggMoves
  ( getEggMoves,
  )
where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text.Encoding as T

getEggMoves :: ByteString -> [Text]
getEggMoves =
  map (T.decodeUtf8 . fromTagText . (!! 1))
    . sections isAttackDexLink
    . takeWhile (~/= ("</main>" :: String))
    . dropWhile (~/= ("<table class=dextab>" :: String))
    . parseTags

isAttackDexLink :: Tag ByteString -> Bool
isAttackDexLink t =
  isTagOpenName "a" t && "attackdex" `BS.isInfixOf` fromAttrib "href" t
