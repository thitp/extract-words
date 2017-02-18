module Data.Text.Words
    ( cleanNumbersAndPunctiation
    , extractWords
    )
where

import Data.Char
import System.IO.Unsafe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.MeCab as MC

mcHandle :: MC.MeCab
mcHandle =
    unsafePerformIO $ MC.new []
{-# NOINLINE mcHandle #-}

mcParse :: T.Text -> [MC.Node T.Text]
mcParse x = unsafePerformIO $ MC.parseToNodes mcHandle x

japWords :: T.Text -> [T.Text]
japWords =
    map MC.nodeSurface . filter ((\x -> x == MC.NOR || x == MC.UNK) . MC.nodeStat) . mcParse

-- | Given a text, extract all words. Also supports japanese input
extractWords :: T.Text -> [T.Text]
extractWords t =
    let cleaned = T.stripStart $ cleanNumbersAndPunctiation t
    in if isJapanese (T.take 1000 cleaned)
          then japWords cleaned
          else filter (not . T.null) . map (T.toLower . T.strip) . T.words $ cleaned

isJapanese :: T.Text -> Bool
isJapanese =
    loop
    where
      jpIdents =
          S.fromList
          "をがはからでだすな漢字注釈出典関連項目"
      loop txt =
          case T.uncons txt of
            Just (c, rest)
                | c `S.member` jpIdents -> True
                | otherwise -> loop rest
            Nothing -> False

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
          | otherwise -> ch
