{-# LANGUAGE QuasiQuotes #-}

import           Data.Text                     (unpack)
import qualified Lib                           as L
import           NeatInterpolation
import           Test.Hspec
import           Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = hspec $ do
  describe "Lib.parseString" $ do
    it "数字と文字だけで構成された文字列をパースできる" $ do
      let s = unpack [text| "abc123" |]
      parse L.parseString "test" s `shouldBe` Right (L.String "abc123")
    it "バックスラッシュを含む文字列をパースできる" $ do
      let s = unpack [text| "abc\123" |]
      parse L.parseString "test" s `shouldBe` Right (L.String "abc\\123")
    it "改行を含む文字列をパースできる" $ do
      let s = "\"abc\n123\""
      parse L.parseString "test" s `shouldBe` Right (L.String "abc\n123")
    it "タブを含む文字列をパースできる" $ do
      let s = "\"abc\t123\""
      parse L.parseString "test" s `shouldBe` Right (L.String "abc\t123")
  describe "Lib.parseNumber" $ do
    it "数字だけで構成された文字列をパースできる" $ do
      let s = "123"
      parse L.parseNumber "test" s `shouldBe` Right (L.Number 123)
    it "#bから始まる2進数の文字列をパースできる" $ do
      let s = "#b111"
      parse L.parseNumber "test" s `shouldBe` Right (L.Number 7)
    it "#oから始まる8進数の文字列をパースできる" $ do
      let s = "#o111"
      parse L.parseNumber "test" s `shouldBe` Right (L.Number 73)
    it "#dから始まる10進数の文字列をパースできる" $ do
      let s = "#d111"
      parse L.parseNumber "test" s `shouldBe` Right (L.Number 111)
    it "#dから始まる10進数の文字列をパースできる" $ do
      let s = "#x111"
      parse L.parseNumber "test" s `shouldBe` Right (L.Number 273)
  describe "Lib.parseChar" $ do
    it "数字の文字をパースできる" $ do
      let s = unpack [text| #\1 |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char '1')
    it "アルファベットの文字をパースできる" $ do
      let s = unpack [text| #\a |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char 'a')
    it "記号の文字をパースできる" $ do
      let s = unpack [text| #\% |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char '%')
    it "newlineの文字をパースできる" $ do
      let s = unpack [text| #\newline |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char '\n')
    it "spaceの文字をパースできる" $ do
      let s = unpack [text| #\space |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char ' ')
    it "何も無い場合にspaceの文字としてパースできる" $ do
      let s = unpack [text| #\ |]
      parse L.parseChar "test" s `shouldBe` Right (L.Char ' ')
  describe "Lib.parseFloat" $ do
    it "小数点を含む数字列をパースできる" $ do
      let s = unpack [text| 123.456 |]
      parse L.parseFloat "test" s `shouldBe` Right (L.Float 123.456)

