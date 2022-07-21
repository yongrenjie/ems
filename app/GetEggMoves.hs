module GetEggMoves ( getEm
  ) where


import qualified Data.ByteString               as BS
import           Data.ByteString                ( ByteString )
import           Data.List                      ( nub
                                                , sort
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as TIO
import           Options                        ( Game(..) )
import           Print
import           Text.HTML.TagSoup
import           Types


getEm :: Game -> ByteString -> [EggMove]
getEm game =
  sort
    . map (extractEm game)
    . filter (isAvailableIn game)
    . partitions isAttackDexLink
    . takeWhile (~/= ("</main>" :: String))
    . dropWhile (~/= ("<table class=dextab>" :: String))
    . parseTags


isAttackDexLink :: Tag ByteString -> Bool
isAttackDexLink t =
  isTagOpenName "a" t && "attackdex" `BS.isInfixOf` fromAttrib "href" t


extractEm :: Game -> [Tag ByteString] -> EggMove
extractEm game ts = EggMove move levelParents breedParents ts'
 where
  move         = T.decodeUtf8 . fromTagText . (!! 1) $ ts
  levelParents = extractLevelParents game ts
  breedParents = extractBreedParents game ts
  ts'          = []


extractLevelParents :: Game -> [Tag ByteString] -> [Pokemon]
extractLevelParents game ts1 = pkmns
 where
  startString = case game of
    SWSH -> "Level Up in Sword & Shield"
    BDSP -> "Level Up in Brilliant Diamond & Shining Pearl"
  endString = case game of
    SWSH -> "Level Up in Brilliant Diamond & Shining Pearl"
    BDSP -> "Parents that Learn through Breeding"
  isThisSectionTag t = case maybeTagText t of
    Nothing -> False
    Just p  -> startString `BS.isInfixOf` p
  isNextSectionTag t = case maybeTagText t of
    Nothing -> False
    Just p  -> endString `BS.isInfixOf` p
  ts2 =
    takeWhile (not . isNextSectionTag) . dropWhile (not . isThisSectionTag) $ ts1
  pkmns = map getPokemonFromImageTag . filter (isTagOpenName "img") $ ts2


extractBreedParents :: Game -> [Tag ByteString] -> [Pokemon]
extractBreedParents game ts1 = pkmns
 where
  startString = case game of
    SWSH -> "Breeding in SWSH"
    BDSP -> "Breeding in BDSP"
  endString = case game of
    SWSH -> "Breeding in BDSP"
    BDSP -> "Learn through Sketch"
  isThisSectionTag t = case maybeTagText t of
    Nothing -> False
    Just p  -> startString `BS.isInfixOf` p
  isNextSectionTag t = case maybeTagText t of
    Nothing -> False
    Just p  -> endString `BS.isInfixOf` p
  ts2 =
    takeWhile (not . isNextSectionTag) . dropWhile (not . isThisSectionTag) $ ts1
  pkmns = map getPokemonFromImageTag . filter (isTagOpenName "img") $ ts2


getPokemonFromImageTag :: Tag ByteString -> Pokemon
getPokemonFromImageTag t = Pokemon (T.strip $ basename <> form)
  where
    basenameRaw = T.decodeUtf8 $ fromAttrib "alt" t
    basename = case T.unsnoc basenameRaw of
                    Just (x, 'B') -> x <> "-M"
                    Just (x, 'A') -> x <> "-F"
                    _ -> basenameRaw
    imageFileName = T.dropEnd 4 . last . T.splitOn "/" . T.decodeUtf8 $ fromAttrib "src" t
    form = case T.splitOn "-" imageFileName of
                [_, x] -> "-" <> T.toUpper x
                _ -> ""


isAvailableIn :: Game -> [Tag ByteString] -> Bool
isAvailableIn g t1 = case dropWhile (~/= ("<i>" :: String)) t1 of
  [] -> True
  t2 -> case innerText (take 3 t2) of
             "BDSP Only" -> g == BDSP
             "SWSH Only" -> g == SWSH
             _           -> True    -- available in both games
