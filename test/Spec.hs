module Main
  ( main
  ) where

import           Sheila
import           Test.Hspec

main :: IO ()
main =
  hspec $
  describe "Sheila.parseCommand" $ do
    describe "default behaviour" $ do
      it "parses empty string" $
        parseCommand "" `shouldBe` Right (PrintCmd DotAddress, "")
      it "parses with address" $
        parseCommand "101\nrest" `shouldBe`
        Right (PrintCmd (LineAddress 101), "rest")
      it "parses without address" $
        parseCommand "\nrest" `shouldBe` Right (PrintCmd DotAddress, "rest")
    describe "addresses" $ do
      it "parses line addresse" $
        parseCommand "123\nrest" `shouldBe`
        Right (PrintCmd (LineAddress 123), "rest")
      it "parses offset addresse" $
        parseCommand "#123\nrest" `shouldBe`
        Right (PrintCmd (OffsetAddress 123), "rest")
      it "parses 0 address" $
        parseCommand "0\nrest" `shouldBe` Right (PrintCmd BeginAddress, "rest")
      it "parses dot address" $
        parseCommand ".\nrest" `shouldBe` Right (PrintCmd DotAddress, "rest")
      it "parses regexp address" $
        parseCommand "/regexp/\nrest" `shouldBe`
        Right (PrintCmd (RegexpAddress "regexp"), "rest")
      it "parses backwards regexp address" $
        parseCommand "?regexp?\nrest" `shouldBe`
        Right
          (PrintCmd (MinusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses file address" $
        parseCommand "\"regexp\"5\nrest" `shouldBe`
        Right (PrintCmd (FileAddress "regexp" (LineAddress 5)), "rest")
    describe "adding" $ do
      it "parses with address" $
        parseCommand "101a/add text/\nrest" `shouldBe`
        Right (AddCmd (LineAddress 101) "add text", "rest")
      it "parses address" $
        parseCommand "a/add text/\nrest" `shouldBe`
        Right (AddCmd DotAddress "add text", "rest")
      it "parses text block" $
        parseCommand "2a\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (AddCmd (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "insert" $ do
      it "parses with address" $
        parseCommand "101i/add text/\nrest" `shouldBe`
        Right (InsertCmd (LineAddress 101) "add text", "rest")
      it "parses address" $
        parseCommand "i/add text/\nrest" `shouldBe`
        Right (InsertCmd DotAddress "add text", "rest")
      it "parses text block" $
        parseCommand "2i\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (InsertCmd (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
