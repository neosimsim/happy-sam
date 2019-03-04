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
        (head . parseCommand) "\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)))
      it "with address" $
        (head . parseCommand) "101\nrest" `shouldBe`
        Right (Print (LineAddress 101))
      it "without address" $
        (head . parseCommand) "\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)))
    describe "addresses" $ do
      it "parses line addresse" $
        (head . parseCommand) "123\nrest" `shouldBe`
        Right (Print (LineAddress 123))
      it "parses offset addresse" $
        (head . parseCommand) "#123\nrest" `shouldBe`
        Right (Print (OffsetAddress 123))
      it "parses 0 address" $
        (head . parseCommand) "0\nrest" `shouldBe` Right (Print ZeroAddress)
      it "parses $ address" $
        (head . parseCommand) "$\nrest" `shouldBe` Right (Print EndAddress)
      it "parses dot address" $
        (head . parseCommand) ".\nrest" `shouldBe` Right (Print DotAddress)
      it "parses regexp address" $
        (head . parseCommand) "/regexp/\nrest" `shouldBe`
        Right (Print (RegexpAddress "regexp"))
      it "parses backwards regexp address" $
        (head . parseCommand) "?regexp?\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (RegexpAddress "regexp")))
      it "parses escaped regexp address" $
        (head . parseCommand) "/\\e\\n\\/[\\/]/\nrest" `shouldBe`
        Right (Print (RegexpAddress "\\e\n/[/]"))
      it "parses escaped backwards regexp address" $
        (head . parseCommand) "?\\e\\n\\?[\\?]?\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (RegexpAddress "\\e\n?[?]")))
      it "parses file address" $
        (head . parseCommand) "\"regexp\"5\nrest" `shouldBe`
        Right (Print (FileAddress "regexp" (LineAddress 5)))
    describe "addresse composition" $ do
      it "parses address translation as default composition" $
        (head . parseCommand) "#5/regexp/2\nrest" `shouldBe`
        Right
          (Print
             (PlusAddress
                (PlusAddress (OffsetAddress 5) (RegexpAddress "regexp"))
                (LineAddress 2)))
      it "parses address translation" $
        (head . parseCommand) "/regexp/+2\nrest" `shouldBe`
        Right (Print (PlusAddress (RegexpAddress "regexp") (LineAddress 2)))
      it "parses reverse address translation" $
        (head . parseCommand) "/regexp/-2\nrest" `shouldBe`
        Right (Print (MinusAddress (RegexpAddress "regexp") (LineAddress 2)))
      it "parses addr+ as addr+1" $
        (head . parseCommand) "/regexp/+\nrest" `shouldBe`
        Right (Print (PlusAddress (RegexpAddress "regexp") (LineAddress 1)))
      it "parses addr- as addr-1" $
        (head . parseCommand) "/regexp/-\nrest" `shouldBe`
        Right (Print (MinusAddress (RegexpAddress "regexp") (LineAddress 1)))
      it "parses +addr as .+addr" $
        (head . parseCommand) "+/regexp/\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (RegexpAddress "regexp")))
      it "parses -addr as .-addr" $
        (head . parseCommand) "-/regexp/\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (RegexpAddress "regexp")))
      it "parses + as .+1" $
        (head . parseCommand) "+\nrest" `shouldBe`
        Right (Print (PlusAddress DotAddress (LineAddress 1)))
      it "parses - as .-1" $
        (head . parseCommand) "-\nrest" `shouldBe`
        Right (Print (MinusAddress DotAddress (LineAddress 1)))
      it "parses address ranges" $
        (head . parseCommand) "/regexp1/,/regexp2/\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress (RegexpAddress "regexp1") (RegexpAddress "regexp2")))
      it "parses relative address ranges" $
        (head . parseCommand) "/regexp1/;/regexp2/\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (RegexpAddress "regexp1")
                (PlusAddress (RegexpAddress "regexp1") (RegexpAddress "regexp2"))))
      it "parses translation before range: ,+3 (precedence rule)" $
        (head . parseCommand) "/regexp1/,/regexp2/+3\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (RegexpAddress "regexp1")
                (PlusAddress (RegexpAddress "regexp2") (LineAddress 3))))
      it "parses translation before range: ;+3 (precedence rule)" $
        (head . parseCommand) "/regexp1/;/regexp2/+3\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (RegexpAddress "regexp1")
                (PlusAddress
                   (RegexpAddress "regexp1")
                   (PlusAddress (RegexpAddress "regexp2") (LineAddress 3)))))
      it "parses translation before range: +3, (precedence rule)" $
        (head . parseCommand) "/regexp1/+3,/regexp2/\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                (RegexpAddress "regexp2")))
      it "parses translation before range: +3; (precedence rule)" $
        (head . parseCommand) "/regexp1/+3;/regexp2/\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                (PlusAddress
                   (PlusAddress (RegexpAddress "regexp1") (LineAddress 3))
                   (RegexpAddress "regexp2"))))
      it "parses 0 as default range start" $
        (head . parseCommand) ",/regexp2/\nrest" `shouldBe`
        Right (Print (RangeAddress ZeroAddress (RegexpAddress "regexp2")))
      it "parses 0 as default relative range start" $
        (head . parseCommand) ";/regexp2/\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                ZeroAddress
                (PlusAddress ZeroAddress (RegexpAddress "regexp2"))))
      it "parses $ as default range start" $
        (head . parseCommand) "/regexp2/,\nrest" `shouldBe`
        Right (Print (RangeAddress (RegexpAddress "regexp2") EndAddress))
      it "parses $ as default relative range end" $
        (head . parseCommand) "/regexp2/;\nrest" `shouldBe`
        Right
          (Print
             (RangeAddress
                (RegexpAddress "regexp2")
                (PlusAddress (RegexpAddress "regexp2") EndAddress)))
      it "parses ',' as whole buffer" $
        (head . parseCommand) ",\nrest" `shouldBe`
        Right (Print (RangeAddress ZeroAddress EndAddress))
      it "parses ';' as whole buffer" $
        (head . parseCommand) ";\nrest" `shouldBe`
        Right
          (Print (RangeAddress ZeroAddress (PlusAddress ZeroAddress EndAddress)))
      it "parses composed file addreses" $
        (head . parseCommand) "\".*rc\"1/regexp/\nrest" `shouldBe`
        Right
          (Print
             (FileAddress
                ".*rc"
                (PlusAddress (LineAddress 1) (RegexpAddress "regexp"))))
    describe "adding" $ do
      it "parses with address" $
        (head . parseCommand) "101a/add text/\nrest" `shouldBe`
        Right (Add (LineAddress 101) "add text")
      it "parses without address" $
        (head . parseCommand) "a/add text/\nrest" `shouldBe`
        Right (Add DotAddress "add text")
      it "parses escaped text lines" $
        (head . parseCommand) "101a/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Add (LineAddress 101) "\\n\nadd/text")
      it "parses text block" $
        (head . parseCommand) "2a\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Add (LineAddress 2) "text\nB.L.O.C.K\n")
    describe "insert" $ do
      it "parses with address" $
        (head . parseCommand) "101i/add text/\nrest" `shouldBe`
        Right (Insert (LineAddress 101) "add text")
      it "parses without address" $
        (head . parseCommand) "i/add text/\nrest" `shouldBe`
        Right (Insert DotAddress "add text")
      it "parses escaped text lines" $
        (head . parseCommand) "101i/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Insert (LineAddress 101) "\\n\nadd/text")
      it "parses text block" $
        (head . parseCommand) "2i\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Insert (LineAddress 2) "text\nB.L.O.C.K\n")
    describe "change" $ do
      it "parses with address" $
        (head . parseCommand) "101c/add text/\nrest" `shouldBe`
        Right (Change (LineAddress 101) "add text")
      it "parses without address" $
        (head . parseCommand) "c/add text/\nrest" `shouldBe`
        Right (Change DotAddress "add text")
      it "parses escaped text lines" $
        (head . parseCommand) "101c/\\\\n\\nadd\\/text/\nrest" `shouldBe`
        Right (Change (LineAddress 101) "\\n\nadd/text")
      it "parses text block" $
        (head . parseCommand) "2c\ntext\nB.L.O.C.K\n.\nrest" `shouldBe`
        Right (Change (LineAddress 2) "text\nB.L.O.C.K\n")
    describe "delete" $ do
      it "parses with address" $
        (head . parseCommand) "101d\nrest" `shouldBe`
        Right (Delete (LineAddress 101))
      it "parses without address" $
        (head . parseCommand) "d\nrest" `shouldBe` Right (Delete DotAddress)
    describe "substitue" $ do
      it "parses with address" $
        (head . parseCommand) "101s/regexp/string/\nrest" `shouldBe`
        Right (Substitute (LineAddress 101) "regexp" "string")
      it "parses without address" $
        (head . parseCommand) "s/regexp/string/\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "string")
      it "parses without closing slash" $
        (head . parseCommand) "s/regexp/string\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "string")
      it "parses without substitution" $
        (head . parseCommand) "s?regexp?\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "")
      it "parses with single slash" $
        (head . parseCommand) "s.regexp\nrest" `shouldBe`
        Right (Substitute DotAddress "regexp" "")
      it "parses escaped text" $
        (head . parseCommand) "s/\\\\n\\nregexp\\//\\\\n\\nstring\\//\nrest" `shouldBe`
        Right (Substitute DotAddress "\\n\nregexp/" "\\n\nstring/")
    describe "move" $ do
      it "parses with address" $
        (head . parseCommand) "101m#4\nrest" `shouldBe`
        Right (Move (LineAddress 101) (OffsetAddress 4))
      it "parses without address" $
        (head . parseCommand) "m#4\nrest" `shouldBe`
        Right (Move DotAddress (OffsetAddress 4))
      it "parses without destination address" $
        (head . parseCommand) "m\nrest" `shouldNotBe`
        Right (Move DotAddress DotAddress)
    describe "copy" $ do
      it "parses with address" $
        (head . parseCommand) "101t#4\nrest" `shouldBe`
        Right (Copy (LineAddress 101) (OffsetAddress 4))
      it "parses without address" $
        (head . parseCommand) "t#4\nrest" `shouldBe`
        Right (Copy DotAddress (OffsetAddress 4))
      it "parses without destination address" $
        (head . parseCommand) "t\nrest" `shouldNotBe`
        Right (Copy DotAddress DotAddress)
    describe "print" $ do
      it "parses with address" $
        (head . parseCommand) "101p\nrest" `shouldBe`
        Right (Print (LineAddress 101))
      it "parses without address" $
        (head . parseCommand) "p\nrest" `shouldBe` Right (Print DotAddress)
    describe "=" $ do
      it "parses with address" $
        (head . parseCommand) "101=\nrest" `shouldBe`
        Right (PrintRange (LineAddress 101))
      it "parses without address" $
        (head . parseCommand) "=\nrest" `shouldBe` Right (PrintRange DotAddress)
    describe "=#" $ do
      it "parses with address" $
        (head . parseCommand) "101=#\nrest" `shouldBe`
        Right (PrintOffset (LineAddress 101))
      it "parses without address" $
        (head . parseCommand) "=#\nrest" `shouldBe`
        Right (PrintOffset DotAddress)
    describe "b" $ do
      it "parses single" $
        (head . parseCommand) "b file\nrest" `shouldBe`
        Right (SetBuffer ["file"])
      it "parses list" $
        (head . parseCommand) "b list of files\nrest" `shouldBe`
        Right (SetBuffer ["list", "of", "files"])
      it "parses pipe" $
        (head . parseCommand) "b   <list of files\nrest" `shouldBe`
        Right (SetBuffer ["<list", "of", "files"])
      it "reports unexpected address" $
        (head . parseCommand) ".b list of files\nrest" `shouldBe`
        Left "command takes no address"
    describe "B" $ do
      it "parses single" $
        (head . parseCommand) "B file\nrest" `shouldBe`
        Right (AddBuffer ["file"])
      it "parses list" $
        (head . parseCommand) "B list of files\nrest" `shouldBe`
        Right (AddBuffer ["list", "of", "files"])
      it "parses pipe" $
        (head . parseCommand) "B   <list of files\nrest" `shouldBe`
        Right (AddBuffer ["<list", "of", "files"])
      it "reports unexpected address" $
        (head . parseCommand) ".B list of files\nrest" `shouldBe`
        Left "command takes no address"
    describe "n" $ do
      it "parses" $ (head . parseCommand) "n\nrest" `shouldBe` Right PrintMenu
      it "reports unexpected address" $
        (head . parseCommand) ".n\nrest" `shouldBe`
        Left "command takes no address"
    describe "D" $ do
      it "parses single" $
        (head . parseCommand) "D file\nrest" `shouldBe`
        Right (DeleteFiles ["file"])
      it "parses list" $
        (head . parseCommand) "D list of files\nrest" `shouldBe`
        Right (DeleteFiles ["list", "of", "files"])
      it "reports unexpected address" $
        (head . parseCommand) ".D list of files\nrest" `shouldBe`
        Left "command takes no address"
    describe "e" $ do
      it "parses single" $
        (head . parseCommand) "e file\nrest" `shouldBe` Right (Edit "file")
      it "reports unexpected address" $
        (head . parseCommand) ".e file\nrest" `shouldBe`
        Left "command takes no address"
    describe "r" $ do
      it "parses with address" $
        (head . parseCommand) "1r file\nrest" `shouldBe`
        Right (Replace (LineAddress 1) "file")
      it "parses without address" $
        (head . parseCommand) "r file\nrest" `shouldBe`
        Right (Replace DotAddress "file")
    describe "w" $ do
      it "parses without address" $
        (head . parseCommand) "w file\nrest" `shouldBe`
        Right (Write (RangeAddress ZeroAddress EndAddress) "file")
      it "parses with address" $
        (head . parseCommand) "#123w file\nrest" `shouldBe`
        Right (Write (OffsetAddress 123) "file")
      it "parses without filename" $
        (head . parseCommand) "w\nrest" `shouldBe`
        Right (Write (RangeAddress ZeroAddress EndAddress) "")
    describe "f" $ do
      it "parses single" $
        (head . parseCommand) "f file\nrest" `shouldBe`
        Right (SetFilename "file")
      it "reports unexpected address" $
        (head . parseCommand) ".f file\nrest" `shouldBe`
        Left "command takes no address"
    describe "<" $ do
      it "parses without address" $
        (head . parseCommand) "< ls -l\nrest" `shouldBe`
        Right (PipeIn DotAddress "ls -l") -- should fail
      it "parses with address" $
        (head . parseCommand) "#123< ls -l\nrest" `shouldBe`
        Right (PipeIn (OffsetAddress 123) "ls -l")
    describe ">" $ do
      it "parses without address" $
        (head . parseCommand) "> ls -l\nrest" `shouldBe`
        Right (PipeOut DotAddress "ls -l") -- should fail
      it "parses with address" $
        (head . parseCommand) "#123> ls -l\nrest" `shouldBe`
        Right (PipeOut (OffsetAddress 123) "ls -l")
    describe "|" $ do
      it "parses without address" $
        (head . parseCommand) "| ls -l\nrest" `shouldBe`
        Right (Pipe DotAddress "ls -l") -- should fail
      it "parses with address" $
        (head . parseCommand) "123| ls -l\nrest" `shouldBe`
        Right (Pipe (LineAddress 123) "ls -l")
    describe "!" $ do
      it "parses" $
        (head . parseCommand) "! ls -l\nrest" `shouldBe`
        Right (RunShell "ls -l")
      it "reports unexpected address" $
        (head . parseCommand) ".! ls\nrest" `shouldBe`
        Left "command takes no address"
    describe "cd" $ do
      it "parses" $
        (head . parseCommand) "cd dir\nrest" `shouldBe` Right (ChangeDir "dir")
      it "reports unexpected address" $
        (head . parseCommand) ". cd dir\nrest" `shouldBe`
        Left "command takes no address"
    describe "x" $ do
      it "parses with address" $
        (head . parseCommand) "1x/regexp/ 4p\nrest" `shouldBe`
        Right (LoopIf (LineAddress 1) "regexp" (Print (LineAddress 4)))
      it "parses without address" $
        (head . parseCommand) "x/regexp/ p\nrest" `shouldBe`
        Right (LoopIf DotAddress "regexp" (Print DotAddress))
      it "parses without command" $
        (head . parseCommand) "x/regexp/\nrest" `shouldBe`
        Right (LoopIf DotAddress "regexp" (Print DotAddress))
      it "parses without regexp" $
        (head . parseCommand) "x 3p\nrest" `shouldBe`
        Right (LoopIf DotAddress ".*\\n" (Print (LineAddress 3)))
      it "parses without any" $
        (head . parseCommand) "x\nrest" `shouldBe`
        Right (LoopIf DotAddress ".*\\n" (Print DotAddress))
    describe "y" $ do
      it "parses with address" $
        (head . parseCommand) "1y/regexp/ 4p\nrest" `shouldBe`
        Right (LoopIfNot (LineAddress 1) "regexp" (Print (LineAddress 4)))
      it "parses without address" $
        (head . parseCommand) "y/regexp/ p\nrest" `shouldBe`
        Right (LoopIfNot DotAddress "regexp" (Print DotAddress))
      it "parses without command" $
        (head . parseCommand) "y/regexp/\nrest" `shouldBe`
        Right (LoopIfNot DotAddress "regexp" (Print DotAddress))
      it "parses without regexp" $
        (head . parseCommand) "y 3p\nrest" `shouldBe`
        Right (LoopIfNot DotAddress ".*\\n" (Print (LineAddress 3)))
      it "parses without any" $
        (head . parseCommand) "y\nrest" `shouldBe`
        Right (LoopIfNot DotAddress ".*\\n" (Print DotAddress))
    describe "X" $ do
      it "parses" $
        (head . parseCommand) "X/regexp/ 4p\nrest" `shouldBe`
        Right (LoopIfFile "regexp" (Print (LineAddress 4)))
      it "parses without regexp" $
        (head . parseCommand) "X 4p\nrest" `shouldBe`
        Right (LoopIfFile "" (Print (LineAddress 4)))
      it "parses without command" $
        (head . parseCommand) "X/regexp/\nrest" `shouldBe`
        Right (LoopIfFile "regexp" (SetFilename ""))
      it "parses without any" $
        (head . parseCommand) "X\nrest" `shouldBe`
        Right (LoopIfFile "" (SetFilename ""))
      it "reports unexpected address" $
        (head . parseCommand) ".X\nrest" `shouldBe`
        Left "command takes no address"
    describe "Y" $ do
      it "parses" $
        (head . parseCommand) "Y/regexp/ 4p\nrest" `shouldBe`
        Right (LoopIfNotFile "regexp" (Print (LineAddress 4)))
      it "parses without regexp" $
        (head . parseCommand) "Y 4p\nrest" `shouldBe`
        Right (LoopIfNotFile "" (Print (LineAddress 4)))
      it "parses without command" $
        (head . parseCommand) "Y/regexp/\nrest" `shouldBe`
        Right (LoopIfNotFile "regexp" (SetFilename ""))
      it "parses without any" $
        (head . parseCommand) "Y\nrest" `shouldBe`
        Right (LoopIfNotFile "" (SetFilename ""))
      it "reports unexpected address" $
        (head . parseCommand) ".Y\nrest" `shouldBe`
        Left "command takes no address"
    describe "g" $ do
      it "parses with address" $
        (head . parseCommand) "1g/regexp/ 4p\nrest" `shouldBe`
        Right (RunIf (LineAddress 1) "regexp" (Print (LineAddress 4)))
      it "parses without address" $
        (head . parseCommand) "g/regexp/ p\nrest" `shouldBe`
        Right (RunIf DotAddress "regexp" (Print DotAddress))
    describe "v" $ do
      it "parses with address" $
        (head . parseCommand) "1v/regexp/ 4p\nrest" `shouldBe`
        Right (RunIfNot (LineAddress 1) "regexp" (Print (LineAddress 4)))
      it "parses without address" $
        (head . parseCommand) "v/regexp/ p\nrest" `shouldBe`
        Right (RunIfNot DotAddress "regexp" (Print DotAddress))
    describe "k" $ do
      it "parses with address" $
        (head . parseCommand) "1k\nrest" `shouldBe` Right (Mark (LineAddress 1))
      it "parses without address" $
        (head . parseCommand) "k\nrest" `shouldBe` Right (Mark DotAddress)
    describe "q" $ do
      it "parses" $ (head . parseCommand) "q\nrest" `shouldBe` Right Quit
      it "reports unexpected address" $
        (head . parseCommand) ".q\nrest" `shouldBe`
        Left "command takes no address"
    describe "u" $ do
      it "parses with number" $
        (head . parseCommand) "u 12\nrest" `shouldBe` Right (Undo 12)
      it "parses without number" $
        (head . parseCommand) "u\nrest" `shouldBe` Right (Undo 1)
      it "reports unexpected address" $
        (head . parseCommand) ".u\nrest" `shouldBe`
        Left "command takes no address"
    describe "composition" $ do
      it "parses nested compositions with address" $
        (head . parseCommand)
          "4{\n    1a/ Hallo Welt /\n    3{\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          (Composed
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
             ])
      it "parses nested compositions without address" $
        (head . parseCommand)
          "{\n    1a/ Hallo Welt /\n    {\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          (Composed
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
             ])
      it "parses nested print address" $
        (head . parseCommand)
          "1{\n    1a/ Hallo Welt /\n    3{\n        2i/hallo/\n        a\ncomposed\ntext\nblock\n.\n3\n    }\n    q\n\n    1i/ Hallo Welt /\n}\nrest" `shouldBe`
        Right
          (Composed
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
             ])
    it "report invalid command line" $
      (head . parseCommand) "H" `shouldBe` Left "invalid command line: H"
