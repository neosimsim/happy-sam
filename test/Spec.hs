module Main
  ( main
  ) where

import           Sheila
import           Test.Hspec

main :: IO ()
main =
  hspec $
  describe "Sheila.parseCommand" $ do
    describe "default behaviour" $ -- TODO maybe introduce Cmd SetDot
     do
      it "parses empty string" $
        parseCommand "\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)), "rest")
      it "with address" $
        parseCommand "101\nrest" `shouldBe`
        Right (Print (LineAddress 101), "rest")
      it "without address" $
        parseCommand "\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)), "rest")
    describe "addresses" $ do
      it "parses line addresse" $
        parseCommand "123\nrest" `shouldBe`
        Right (Print (LineAddress 123), "rest")
      it "parses offset addresse" $
        parseCommand "#123\nrest" `shouldBe`
        Right (Print (OffsetAddress 123), "rest")
      it "parses 0 address" $
        parseCommand "0\nrest" `shouldBe` Right (Print ZeroAddress, "rest")
      it "parses $ address" $
        parseCommand "$\nrest" `shouldBe` Right (Print EndAddress, "rest")
      it "parses dot address" $
        parseCommand ".\nrest" `shouldBe` Right (Print DotAddress, "rest")
      it "parses regexp address" $
        parseCommand "/regexp/\nrest" `shouldBe`
        Right (Print (RegexpAddress "regexp"), "rest")
      it "parses backwards regexp address" $
        parseCommand "?regexp?\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses escaped regexp address" $
        parseCommand "/\\e\\n\\/[\\/]/\nrest" `shouldBe`
        Right (Print (RegexpAddress "\\e\n/[/]"), "rest")
      it "parses escaped backwards regexp address" $
        parseCommand "?\\e\\n\\?[\\?]?\nrest" `shouldBe`
        Right
          (Print (MinusAddress DotAddress (RegexpAddress "\\e\n?[?]")), "rest")
      it "parses file address" $
        parseCommand "\"regexp\"5\nrest" `shouldBe`
        Right (Print (FileAddress "regexp" (LineAddress 5)), "rest")
    describe "addresse composition" $ do
      it "parses address translation as default composition" $
        parseCommand "#5/regexp/2\nrest" `shouldBe`
        Right
          ( Print
              (PlusAddress
                 (PlusAddress (OffsetAddress 5) (RegexpAddress "regexp"))
                 (LineAddress 2))
          , "rest")
      it "parses address translation" $
        parseCommand "/regexp/+2\nrest" `shouldBe`
        Right
          (Print (PlusAddress (RegexpAddress "regexp") (LineAddress 2)), "rest")
      it "parses reverse address translation" $
        parseCommand "/regexp/-2\nrest" `shouldBe`
        Right
          ( Print (MinusAddress (RegexpAddress "regexp") (LineAddress 2))
          , "rest")
      it "parses addr+ as addr+1" $
        parseCommand "/regexp/+\nrest" `shouldBe`
        Right
          (Print (PlusAddress (RegexpAddress "regexp") (LineAddress 1)), "rest")
      it "parses addr- as addr-1" $
        parseCommand "/regexp/-\nrest" `shouldBe`
        Right
          ( Print (MinusAddress (RegexpAddress "regexp") (LineAddress 1))
          , "rest")
      it "parses +addr as .+addr" $
        parseCommand "+/regexp/\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses -addr as .-addr" $
        parseCommand "-/regexp/\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (RegexpAddress "regexp")), "rest")
      it "parses + as .+1" $
        parseCommand "+\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)), "rest")
      it "parses - as .-1" $
        parseCommand "-\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (LineAddress 1)), "rest")
      it "parses address ranges" $
        parseCommand "/regexp1/,/regexp2/\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress (RegexpAddress "regexp1") (RegexpAddress "regexp2"))
          , "rest")
      it "parses relative address ranges" $
        parseCommand "/regexp1/;/regexp2/\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress
                    (RegexpAddress "regexp1")
                    (RegexpAddress "regexp2")))
          , "rest")
      it "parses translation before range: ,+3 (precedence rule)" $
        parseCommand "/regexp1/,/regexp2/+3\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress (RegexpAddress "regexp2") (LineAddress 3)))
          , "rest")
      it "parses translation before range: ;+3 (precedence rule)" $
        parseCommand "/regexp1/;/regexp2/+3\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (RegexpAddress "regexp1")
                 (PlusAddress
                    (RegexpAddress "regexp1")
                    (PlusAddress (RegexpAddress "regexp2") (LineAddress 3))))
          , "rest")
      it "parses translation before range: +3, (precedence rule)" $
        parseCommand "/regexp1/+3,/regexp2/\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                 (RegexpAddress "regexp2"))
          , "rest")
      it "parses translation before range: +3; (precedence rule)" $
        parseCommand "/regexp1/+3;/regexp2/\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                 (PlusAddress
                    (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                    (RegexpAddress "regexp2")))
          , "rest")
      it "parses 0 as default range start" $
        parseCommand ",/regexp2/\nrest" `shouldBe`
        Right
          (Print (RangeAddress ZeroAddress (RegexpAddress "regexp2")), "rest")
      it "parses 0 as default relative range start" $
        parseCommand ";/regexp2/\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 ZeroAddress
                 (PlusAddress ZeroAddress (RegexpAddress "regexp2")))
          , "rest")
      it "parses $ as default range start" $
        parseCommand "/regexp2/,\nrest" `shouldBe`
        Right
          (Print (RangeAddress (RegexpAddress "regexp2") EndAddress), "rest")
      it "parses $ as default relative range end" $
        parseCommand "/regexp2/;\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress
                 (RegexpAddress "regexp2")
                 (PlusAddress (RegexpAddress "regexp2") EndAddress))
          , "rest")
      it "parses ',' as whole buffer" $
        parseCommand ",\nrest" `shouldBe`
        Right (Print (RangeAddress ZeroAddress EndAddress), "rest")
      it "parses ';' as whole buffer" $
        parseCommand ";\nrest" `shouldBe`
        Right
          ( Print
              (RangeAddress ZeroAddress (PlusAddress ZeroAddress EndAddress))
          , "rest")
      it "parses composed file addreses" $
        parseCommand "\".*rc\"1/regexp/\nrest" `shouldBe`
        Right
          ( Print
              (FileAddress
                 ".*rc"
                 (PlusAddress (LineAddress 1) (RegexpAddress "regexp")))
          , "rest")
    describe "adding" $ do
      it "parses with address" $
        parseCommand "101a/add text/\nrest" `shouldBe`
        Right (Add (LineAddress 101) "add text", "rest")
      it "parses without address" $
        parseCommand "a/add text/\nrest" `shouldBe`
        Right (Add DotAddress "add text", "rest")
      it "parses escaped text lines" $
        parseCommand "101a/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Add (LineAddress 101) "\\n\nadd/text", "rest")
      it "parses text block" $
        parseCommand "2a\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Add (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "insert" $ do
      it "parses with address" $
        parseCommand "101i/add text/\nrest" `shouldBe`
        Right (Insert (LineAddress 101) "add text", "rest")
      it "parses without address" $
        parseCommand "i/add text/\nrest" `shouldBe`
        Right (Insert DotAddress "add text", "rest")
      it "parses escaped text lines" $
        parseCommand "101i/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Insert (LineAddress 101) "\\n\nadd/text", "rest")
      it "parses text block" $
        parseCommand "2i\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Insert (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "change" $ do
      it "parses with address" $
        parseCommand "101c/add text/\nrest" `shouldBe`
        Right (Change (LineAddress 101) "add text", "rest")
      it "parses without address" $
        parseCommand "c/add text/\nrest" `shouldBe`
        Right (Change DotAddress "add text", "rest")
      it "parses escaped text lines" $
        parseCommand "101c/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Change (LineAddress 101) "\\n\nadd/text", "rest")
      it "parses text block" $
        parseCommand "2c\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Change (LineAddress 2) "text\nB.L.O.C.K\n", "rest")
    describe "delete" $ do
      it "parses with address" $
        parseCommand "101d\nrest" `shouldBe`
        Right (Delete (LineAddress 101), "rest")
      it "parses without address" $
        parseCommand "d\nrest" `shouldBe` Right (Delete DotAddress, "rest")
    describe "substitue" $ do
      it "parses with address" $
        parseCommand "101s/regexp/string/\nrest" `shouldBe`
        Right (Substitute (LineAddress 101) "regexp" "string", "rest")
      it "parses without address" $
        parseCommand "s/regexp/string/\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "string", "rest")
      it "parses without closing slash" $
        parseCommand "s/regexp/string\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "string", "rest")
      it "parses without substitution" $
        parseCommand "s?regexp?\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "", "rest")
      it "parses with single slash" $
        parseCommand "s.regexp\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "", "rest")
      it "parses escaped text" $
        parseCommand "s/\\\\n\\nregexp\\//\\\\n\\nstring\\//\nrest" `shouldBe`
        Right (Substitute DotAddress "\\n\nregexp/" "\\n\nstring/", "rest")
    describe "move" $ do
      it "parses with address" $
        parseCommand "101m#4\nrest" `shouldBe`
        Right (Move (LineAddress 101) (OffsetAddress 4), "rest")
      it "parses without address" $
        parseCommand "m#4\nrest" `shouldBe`
        Right (Move DotAddress (OffsetAddress 4), "rest")
      it "parses without destination address" $
        parseCommand "m\nrest" `shouldNotBe`
        Right (Move DotAddress DotAddress, "rest")
    describe "copy" $ do
      it "parses with address" $
        parseCommand "101t#4\nrest" `shouldBe`
        Right (Copy (LineAddress 101) (OffsetAddress 4), "rest")
      it "parses without address" $
        parseCommand "t#4\nrest" `shouldBe`
        Right (Copy DotAddress (OffsetAddress 4), "rest")
      it "parses without destination address" $
        parseCommand "t\nrest" `shouldNotBe`
        Right (Copy DotAddress DotAddress, "rest")
    describe "print" $ do
      it "parses with address" $
        parseCommand "101p\nrest" `shouldBe`
        Right (Print (LineAddress 101), "rest")
      it "parses without address" $
        parseCommand "p\nrest" `shouldBe` Right (Print DotAddress, "rest")
    describe "=" $ do
      it "parses with address" $
        parseCommand "101=\nrest" `shouldBe`
        Right (PrintRange (LineAddress 101), "rest")
      it "parses without address" $
        parseCommand "=\nrest" `shouldBe` Right (PrintRange DotAddress, "rest")
    describe "=#" $ do
      it "parses with address" $
        parseCommand "101=#\nrest" `shouldBe`
        Right (PrintOffset (LineAddress 101), "rest")
      it "parses without address" $
        parseCommand "=#\nrest" `shouldBe`
        Right (PrintOffset DotAddress, "rest")
    describe "b" $ do
      it "parses single" $
        parseCommand "b file\nrest" `shouldBe`
        Right (SetBuffer ["file"], "rest")
      it "parses list" $
        parseCommand "b list of files\nrest" `shouldBe`
        Right (SetBuffer ["list", "of", "files"], "rest")
      it "parses pipe" $
        parseCommand "b   <list of files\nrest" `shouldBe`
        Right (SetBuffer ["<list", "of", "files"], "rest")
    describe "B" $ do
      it "parses single" $
        parseCommand "B file\nrest" `shouldBe`
        Right (AddBuffer ["file"], "rest")
      it "parses list" $
        parseCommand "B list of files\nrest" `shouldBe`
        Right (AddBuffer ["list", "of", "files"], "rest")
      it "parses pipe" $
        parseCommand "B   <list of files\nrest" `shouldBe`
        Right (AddBuffer ["<list", "of", "files"], "rest")
    describe "n" $
      it "parses" $ parseCommand "n\nrest" `shouldBe` Right (PrintMenu, "rest")
    describe "D" $ do
      it "parses single" $
        parseCommand "D file\nrest" `shouldBe`
        Right (DeleteFiles ["file"], "rest")
      it "parses list" $
        parseCommand "D list of files\nrest" `shouldBe`
        Right (DeleteFiles ["list", "of", "files"], "rest")
    describe "e" $
      it "parses single" $
      parseCommand "e file\nrest" `shouldBe` Right (Edit "file", "rest")
    describe "r" $ do
      it "parses with address" $
        parseCommand "1r file\nrest" `shouldBe`
        Right (Replace (LineAddress 1) "file", "rest")
      it "parses without address" $
        parseCommand "r file\nrest" `shouldBe`
        Right (Replace DotAddress "file", "rest")
    describe "w" $ do
      it "parses without address" $
        parseCommand "w file\nrest" `shouldBe`
        Right (Write (RangeAddress ZeroAddress EndAddress) "file", "rest")
      it "parses with address" $
        parseCommand "#123w file\nrest" `shouldBe`
        Right (Write (OffsetAddress 123) "file", "rest")
      it "parses without filename" $
        parseCommand "w\nrest" `shouldBe`
        Right (Write (RangeAddress ZeroAddress EndAddress) "", "rest")
    describe "f" $
      it "parses single" $
      parseCommand "f file\nrest" `shouldBe` Right (SetFilename "file", "rest")
    describe "<" $ do
      it "parses without address" $
        parseCommand "< ls -l\nrest" `shouldBe`
        Right (PipeIn DotAddress "ls -l", "rest") -- should fail
      it "parses with address" $
        parseCommand "#123< ls -l\nrest" `shouldBe`
        Right (PipeIn (OffsetAddress 123) "ls -l", "rest")
    describe ">" $ do
      it "parses without address" $
        parseCommand "> ls -l\nrest" `shouldBe`
        Right (PipeOut DotAddress "ls -l", "rest") -- should fail
      it "parses with address" $
        parseCommand "#123> ls -l\nrest" `shouldBe`
        Right (PipeOut (OffsetAddress 123) "ls -l", "rest")
    describe "|" $ do
      it "parses without address" $
        parseCommand "| ls -l\nrest" `shouldBe`
        Right (Pipe DotAddress "ls -l", "rest") -- should fail
      it "parses with address" $
        parseCommand "123| ls -l\nrest" `shouldBe`
        Right (Pipe (LineAddress 123) "ls -l", "rest")
    describe "!" $
      it "parses" $
      parseCommand "! ls -l\nrest" `shouldBe` Right (RunShell "ls -l", "rest")
    describe "cd" $
      it "parses" $
      parseCommand "cd dir\nrest" `shouldBe` Right (ChangeDir "dir", "rest")
    describe "x" $ do
      it "parses with address" $
        parseCommand "1x/regexp/ 4p\nrest" `shouldBe`
        Right (LoopIf (LineAddress 1) "regexp" (Print (LineAddress 4)), "rest")
      it "parses without address" $
        parseCommand "x/regexp/ p\nrest" `shouldBe`
        Right (LoopIf DotAddress "regexp" (Print DotAddress), "rest")
    describe "y" $ do
      it "parses with address" $
        parseCommand "1y/regexp/ 4p\nrest" `shouldBe`
        Right
          (LoopIfNot (LineAddress 1) "regexp" (Print (LineAddress 4)), "rest")
      it "parses without address" $
        parseCommand "y/regexp/ p\nrest" `shouldBe`
        Right (LoopIfNot DotAddress "regexp" (Print DotAddress), "rest")
    describe "X" $
      it "parses" $
      parseCommand "X/regexp/ 4p\nrest" `shouldBe`
      Right (LoopIfFile "regexp" (Print (LineAddress 4)), "rest")
    describe "Y" $
      it "parses" $
      parseCommand "Y/regexp/ 4p\nrest" `shouldBe`
      Right (LoopIfNotFile "regexp" (Print (LineAddress 4)), "rest")
    describe "g" $ do
      it "parses with address" $
        parseCommand "1g/regexp/ 4p\nrest" `shouldBe`
        Right (RunIf (LineAddress 1) "regexp" (Print (LineAddress 4)), "rest")
      it "parses without address" $
        parseCommand "g/regexp/ p\nrest" `shouldBe`
        Right (RunIf DotAddress "regexp" (Print DotAddress), "rest")
    describe "v" $ do
      it "parses with address" $
        parseCommand "1v/regexp/ 4p\nrest" `shouldBe`
        Right
          (RunIfNot (LineAddress 1) "regexp" (Print (LineAddress 4)), "rest")
      it "parses without address" $
        parseCommand "v/regexp/ p\nrest" `shouldBe`
        Right (RunIfNot DotAddress "regexp" (Print DotAddress), "rest")
    describe "k" $ do
      it "parses with address" $
        parseCommand "1k\nrest" `shouldBe` Right (Mark (LineAddress 1), "rest")
      it "parses without address" $
        parseCommand "k\nrest" `shouldBe` Right (Mark DotAddress, "rest")
    describe "q" $
      it "parses" $ parseCommand "q\nrest" `shouldBe` Right (Quit, "rest")
    describe "u" $ do
      it "parses with number" $
        parseCommand "u 12\nrest" `shouldBe` Right (Undo 12, "rest")
      it "parses without number" $
        parseCommand "u\nrest" `shouldBe` Right (Undo 1, "rest")
    describe "composition" $ do
      it "parses nested compositions with address" $
        parseCommand
          "4{\n    1a/ Hallo Welt /\n    3{\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          ( Composed
              (LineAddress 4)
              [ Add (LineAddress 1) " Hallo Welt "
              , Composed
                  (LineAddress 3)
                  [ Insert (LineAddress 2) "hallo"
                  , Add DotAddress "composed\ntext\nblock\n"
                  , Print (PlusAddress DotAddress (LineAddress 1))
                  ]
              , Quit
              , Print (PlusAddress DotAddress (LineAddress 1))
              , Insert (LineAddress 1) " Hallo Welt "
              ]
          , "rest")
      it "parses nested compositions without address" $
        parseCommand
          "{\n    1a/ Hallo Welt /\n    {\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          ( Composed
              DotAddress
              [ Add (LineAddress 1) " Hallo Welt "
              , Composed
                  DotAddress
                  [ Insert (LineAddress 2) "hallo"
                  , Add DotAddress "composed\ntext\nblock\n"
                  ]
              , Quit
              , Print (PlusAddress DotAddress (LineAddress 1))
              , Insert (LineAddress 1) " Hallo Welt "
              ]
          , "rest")
      it "parses nested print address" $
        parseCommand
          "1{\n    1a/ Hallo Welt /\n    3{\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n3\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          ( Composed
              (LineAddress 1)
              [ Add (LineAddress 1) " Hallo Welt "
              , Composed
                  (LineAddress 3)
                  [ Insert (LineAddress 2) "hallo"
                  , Add DotAddress "composed\ntext\nblock\n"
                  , Print (LineAddress 3)
                  ]
              , Quit
              , Print (PlusAddress DotAddress (LineAddress 1))
              , Insert (LineAddress 1) " Hallo Welt "
              ]
          , "rest")
