module Main
  ( main
  ) where

import           Sheila.Parser

main :: IO ()
main = getContents >>= parseAndPrint

parseAndPrint :: String -> IO ()
parseAndPrint s =
  case parseCommand s of
    Left e -> putStrLn e
    Right (cmd, rest) -> do
      print cmd
      case rest of
        [] -> return ()
        _  -> parseAndPrint rest
