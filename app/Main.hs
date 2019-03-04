module Main
  ( main
  ) where

import           Sheila

main :: IO ()
main = getContents >>= mapM_ print . parseCommand
