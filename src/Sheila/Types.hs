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
  | RegexpAddress String
  | OffsetAddress Int
  | LineAddress Int
  | ZeroAddress
  | EndAddress
  | MarkedAddress
  | FileAddress String
                Address
  | PlusAddress Address
                Address
  | MinusAddress Address
                 Address
  | RangeAddress Address
                 Address
  deriving (Show, Eq)
