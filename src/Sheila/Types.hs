module Sheila.Types
  ( Cmd(..)
  , Address(..)
  ) where

data Cmd
  = PrintCmd Address
  | AddCmd Address
           String
  | InsertCmd Address
              String
  | QuitCmd
  | ComposedCmd Address
                [Cmd]
  deriving (Show, Eq)

data Address
  = DotAddress
  | RegexAddress String
  | OffsetAddress Int
  | LineAddress Int
  | ComposedAddress Address
                    Address
  deriving (Show, Eq)
