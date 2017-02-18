{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Words

import Test.Hspec

main :: IO ()
main =
    hspec $
    do describe "word splitting" $
           do it "should work for german" $
                  extractWords "Hallo, das ist ein deutscher Text." `shouldBe`
                  ["hallo", "das", "ist", "ein", "deutscher", "text"]
              it "should work for japanese" $
                  extractWords "これは、日本語のテキストです。" `shouldBe`
                  ["これ", "は", "日本語", "の", "テキスト", "です"]
