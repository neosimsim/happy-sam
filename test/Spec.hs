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
        parseCommand "0\nrest" `shouldBe` Right (PrintCmd ZeroAddress, "rest")
      it "parses $ address" $
        parseCommand "$\nrest" `shouldBe` Right (PrintCmd EndAddress, "rest")
      it "parses dot address" $
        parseCommand ".\nrest" `shouldBe` Right (PrintCmd DotAddress, "rest")
      it "parses regexp address" $
        parseCommand "/regexp/\nrest" `shouldBe`
        Right (PrintCmd (RegexpAddress "regexp"), "rest")
      it "parses backwards regexp address" $
        parseCommand "?regexp?\nrest" `shouldBe`
        Right
          (PrintCmd (MinusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses escaped regexp address" $
        parseCommand "/\\e\\n\\/[\\/]/\nrest" `shouldBe`
        Right (PrintCmd (RegexpAddress "\\e\n/[/]"), "rest")
      it "parses escaped backwards regexp address" $
        parseCommand "?\\e\\n\\?[\\?]?\nrest" `shouldBe`
        Right
          ( PrintCmd (MinusAddress DotAddress (RegexpAddress "\\e\n?[?]"))
          , "rest")
      it "parses file address" $
        parseCommand "\"regexp\"5\nrest" `shouldBe`
        Right (PrintCmd (FileAddress "regexp" (LineAddress 5)), "rest")
    describe "addresse composition" $ do
      it "parses address translation as default composition" $
        parseCommand "#5/regexp/2\nrest" `shouldBe`
        Right
          ( PrintCmd
              (PlusAddress
                 (PlusAddress (OffsetAddress 5) (RegexpAddress "regexp"))
                 (LineAddress 2))
          , "rest")
      it "parses address translation" $
        parseCommand "/regexp/+2\nrest" `shouldBe`
        Right
          ( PrintCmd (PlusAddress (RegexpAddress "regexp") (LineAddress 2))
          , "rest")
      it "parses reverse address translation" $
        parseCommand "/regexp/-2\nrest" `shouldBe`
        Right
          ( PrintCmd (MinusAddress (RegexpAddress "regexp") (LineAddress 2))
          , "rest")
      it "parses addr+ as addr+1" $
        parseCommand "/regexp/+\nrest" `shouldBe`
        Right
          ( PrintCmd (PlusAddress (RegexpAddress "regexp") (LineAddress 1))
          , "rest")
      it "parses addr- as addr-1" $
        parseCommand "/regexp/-\nrest" `shouldBe`
        Right
          ( PrintCmd (MinusAddress (RegexpAddress "regexp") (LineAddress 1))
          , "rest")
      it "parses +addr as .+addr" $
        parseCommand "+/regexp/\nrest" `shouldBe`
        Right
          (PrintCmd (PlusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses -addr as .-addr" $
        parseCommand "-/regexp/\nrest" `shouldBe`
        Right
          (PrintCmd (MinusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses + as .+1" $
        parseCommand "+\nrest" `shouldBe`
        Right (PrintCmd (PlusAddress DotAddress (LineAddress 1)), "rest")
      it "parses - as .-1" $
        parseCommand "-\nrest" `shouldBe`
        Right (PrintCmd (MinusAddress DotAddress (LineAddress 1)), "rest")
      it "parses address ranges" $
        parseCommand "/regexp1/,/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress (RegexpAddress "regexp1") (RegexpAddress "regexp2"))
          , "rest")
      it "parses relative address ranges" $
        parseCommand "/regexp1/;/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress
                    (RegexpAddress "regexp1")
                    (RegexpAddress "regexp2")))
          , "rest")
      it "parses translation before range: ,+3 (precedence rule)" $
        parseCommand "/regexp1/,/regexp2/+3\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress (RegexpAddress "regexp2") (LineAddress 3)))
          , "rest")
      it "parses translation before range: ;+3 (precedence rule)" $
        parseCommand "/regexp1/;/regexp2/+3\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress
                    (RegexpAddress "regexp1")
                    (PlusAddress (RegexpAddress "regexp2") (LineAddress 3))))
          , "rest")
      it "parses translation before range: +3, (precedence rule)" $
        parseCommand "/regexp1/+3,/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                 (RegexpAddress "regexp2"))
          , "rest")
      it "parses translation before range: +3; (precedence rule)" $
        parseCommand "/regexp1/+3;/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                 (PlusAddress
                    (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                    (RegexpAddress "regexp2")))
          , "rest")
      it "parses 0 as default range start" $
        parseCommand ",/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd (RangeAddress ZeroAddress (RegexpAddress "regexp2"))
          , "rest")
      it "parses 0 as default relative range start" $
        parseCommand ";/regexp2/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 ZeroAddress
                 (PlusAddress ZeroAddress (RegexpAddress "regexp2")))
          , "rest")
      it "parses $ as default range start" $
        parseCommand "/regexp2/,\nrest" `shouldBe`
        Right
          (PrintCmd (RangeAddress (RegexpAddress "regexp2") EndAddress), "rest")
      it "parses $ as default relative range end" $
        parseCommand "/regexp2/;\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress
                 (RegexpAddress "regexp2")
                 (PlusAddress (RegexpAddress "regexp2") EndAddress))
          , "rest")
      it "parses ',' as whole buffer" $
        parseCommand ",\nrest" `shouldBe`
        Right (PrintCmd (RangeAddress ZeroAddress EndAddress), "rest")
      it "parses ';' as whole buffer" $
        parseCommand ";\nrest" `shouldBe`
        Right
          ( PrintCmd
              (RangeAddress ZeroAddress (PlusAddress ZeroAddress EndAddress))
          , "rest")
      it "parses composed file addreses" $
        parseCommand "\".*rc\"1/regexp/\nrest" `shouldBe`
        Right
          ( PrintCmd
              (FileAddress
                 ".*rc"
                 (PlusAddress (LineAddress 1) (RegexpAddress "regexp")))
          , "rest")
    describe "adding" $ do
      it "parses with address" $
        parseCommand "101a/add text/\nrest" `shouldBe`
        Right (AddCmd (LineAddress 101) "add text", "rest")
      it "parses without address" $
        parseCommand "a/add text/\nrest" `shouldBe`
        Right (AddCmd DotAddress "add text", "rest")
      it "parses escaped text lines" $
        parseCommand "101a/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (AddCmd (LineAddress 101) "\\n\nadd/text", "rest")
      it "parses text block" $
        parseCommand "2a\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (AddCmd (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "insert" $ do
      it "parses with address" $
        parseCommand "101i/add text/\nrest" `shouldBe`
        Right (InsertCmd (LineAddress 101) "add text", "rest")
      it "parses without address" $
        parseCommand "i/add text/\nrest" `shouldBe`
        Right (InsertCmd DotAddress "add text", "rest")
      it "parses escaped text lines" $
        parseCommand "101i/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (InsertCmd (LineAddress 101) "\\n\nadd/text", "rest")
      it "parses text block" $
        parseCommand "2i\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (InsertCmd (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "composition" $
      it "parses nested compositions" $
      parseCommand
        "1{\n    1a/ Hallo Welt /\n    3{\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
      Right
        ( ComposedCmd
            (LineAddress 1)
            [ AddCmd (LineAddress 1) " Hallo Welt "
            , ComposedCmd
                (LineAddress 3)
                [ InsertCmd (LineAddress 2) "hallo"
                , AddCmd DotAddress "composed\ntext\nblock\n"
                ]
            , QuitCmd
            , PrintCmd DotAddress
            , InsertCmd (LineAddress 1) " Hallo Welt "
            ]
        , "rest")
