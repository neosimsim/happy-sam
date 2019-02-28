module Sheila.Types
  ( Cmd(..)
  , Address(..)
  ) where

-- | Data type to compose sam commands.
data Cmd
  = Add Address -- ^ Text Commands
        String
  | Insert Address
           String
  | Change Address
           String
  | Delete Address
  | Substitute Address
               String
               String
  | Move Address
         Address
  | Copy Address
         Address
  | Print Address -- ^ Display Commands
  | PrintRange Address
  | PrintOffset Address
  | SetBuffer [String] -- ^ File Commands
  | AddBuffer [String]
  | PrintMenu
  | DeleteFiles [String]
  | Edit String -- ^ I/O Commands
  | Replace Address
            String
  | Write Address
          String
  | SetFilename String
  | PipeIn Address
           String
  | PipeOut Address
            String
  | Pipe Address
         String
  | RunShell String
  | ChangeDir String
  | LoopIf Address -- ^ Loops and Conditionals
           String
           Cmd
  | LoopIfNot Address
              String
              Cmd
  | LoopIfFile String
               Cmd
  | LoopIfNotFile String
                  Cmd
  | RunIf Address
          String
          Cmd
  | RunIfNot Address
             String
             Cmd
  | Mark Address -- ^ Misc
  | Quit
  | Undo Int
  | Composed Address
             [Cmd]
  deriving (Show, Eq)

-- | Types to compose addresses and ranges in a text.
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
