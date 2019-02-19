module Main
  ( main
  ) where

import           Sheila.Parser

main :: IO ()
main = getContents >>= print . parseFoo
