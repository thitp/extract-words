{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Words

import Test.Hspec
import qualified Data.Vector as V

main :: IO ()
main =
    hspec $
    do describe "word splitting" $
           do it "should work for german" $
                  extractWords' "Hallo, das ist ein deutscher Text." `shouldBe`
                  ( []
                  , V.fromList ["hallo", "das", "ist", "ein", "deutscher", "text"]
                  )
              it "should work for japanese" $
                  extractWords' "これは、日本語のテキストです。" `shouldBe`
                  ( [EContainsJapanese]
                  , V.fromList ["これ", "は", "日本語", "の", "テキスト", "です"]
                  )
              it "should work for mixed german and japanese texts" $
                  extractWords' "Hallo mein Freund: これは、Ok 日本語のテキストです。" `shouldBe`
                  ( [EContainsJapanese]
                  , V.fromList ["Hallo", "mein", "Freund", "これ", "は", "Ok", "日本語", "の", "テキスト", "です"]
                  )
              it "should work for mixed english and japanese" $
                  extractWords' "曲げR or　曲げアール" `shouldBe`
                  ( [EContainsJapanese]
                  , V.fromList ["曲げ", "R", "or", "曲げ", "アール"]
                  )
