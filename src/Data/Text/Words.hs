{-# LANGUAGE ViewPatterns #-}
module Data.Text.Words
    ( cleanNumbersAndPunctiation
    , extractWords, extractWords', ExtractInfo(..)
    , TextInfo(..)
    , containsJapanese
    )
where

import Data.Char
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.MeCab as MC

mcParse :: T.Text -> [MC.Node T.Text]
mcParse x =
    unsafePerformIO $
    do mcHandle <- MC.new []
       MC.parseToNodes mcHandle x

japWords :: T.Text -> V.Vector T.Text
japWords =
    V.map MC.nodeSurface . V.filter ((\x -> x == MC.NOR || x == MC.UNK) . MC.nodeStat)
    . V.fromList . mcParse

-- | Given a text, extract all words. Also supports japanese input
extractWords :: TextInfo -> T.Text -> V.Vector T.Text
extractWords ti = snd . extractWords' ti

data ExtractInfo
    = EContainsJapanese
    deriving (Show, Eq)

data TextInfo
    = TiPossiblyJapanese
    | TiNoJapanese
    deriving (Show, Eq)

-- | Given a text, extract all words and provides additional information
-- found during extraction
extractWords' :: TextInfo -> T.Text -> ([ExtractInfo], V.Vector T.Text)
extractWords' ti t =
    let cleaned = T.stripStart $ cleanNumbersAndPunctiation t
        hasJp = ti == TiPossiblyJapanese && containsJapanese cleaned
        wrds =
            if hasJp
            then japWords cleaned
            else V.filter (not . T.null) . V.map (T.toLower . T.strip) . V.fromList . T.words $ cleaned
    in ( if hasJp then [EContainsJapanese] else []
       , wrds
       )

-- | does the given text contain japanese charaters
containsJapanese :: T.Text -> Bool
containsJapanese =
    loop
    where
      loop txt =
          case T.uncons txt of
            Just (c, rest)
                | isJap c -> True
                | otherwise -> loop rest
            Nothing -> False

isJapPunct :: Char -> Bool
isJapPunct (ord -> c) =
    c >= 0x3000 && c <= 0x303f

isJap :: Char -> Bool
isJap o@(ord -> c) =
    isJapPunct o
    || (c >= 0x3040 && c <= 0x309f) -- hiragana
    || (c >= 0x30a0 && c <= 0x30ff) -- katakana
    || (c >= 0xff00 && c <= 0xffef) -- full width roman
    || (c >= 0x4e00 && c <= 0x9faf) -- kanji

-- | Replace all punctiation and numbers from input and replace with
-- space
cleanNumbersAndPunctiation :: T.Text -> T.Text
cleanNumbersAndPunctiation =
    T.map $ \ch ->
    case ch of
      '?' -> ' '
      '[' -> ' '
      ']' -> ' '
      '(' -> ' '
      ')' -> ' '
      '<' -> ' '
      '>' -> ' '
      '#' -> ' '
      '+' -> ' '
      '$' -> ' '
      '€' -> ' '
      '!' -> ' '
      '.' -> ' '
      ',' -> ' '
      ':' -> ' '
      ';' -> ' '
      '。' -> ' '
      '、' -> ' '
      _
          | isNumber ch -> ' '
          | isPunctuation ch -> ' '
          | isSymbol ch -> ' '
          | ord ch == 0x3000 -> ' ' -- full-width space
          | otherwise -> ch
