module Main
  ( main
  ) where

import           Sheila
import           Test.Hspec

main :: IO ()
main =
  hspec $
  describe "Sheia.parseCommand" $ do
    it "allows add with address" $
      parseCommand "101a/add text/" `shouldBe`
      Right (AddCmd (LineAddress 101) "add text", "")
    it "allows add with default dot address" $
      parseCommand "a/add text/" `shouldBe`
      Right (AddCmd DotAddress "add text", "")
