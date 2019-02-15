module Main
  ( main
  ) where

import           Sheila.Parser

main :: IO ()
main = getContents >>= return . parseFoo . lexer >>= mapM_ print
