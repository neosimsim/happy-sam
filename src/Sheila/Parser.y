{
module Sheila.Parser (parseFoo, lexer) where

import Data.Char
}

%name parseFoo
%tokentype { Token }
%error { parseError }

%token
      'a'     { TokenAdd }
      'q'     { TokenQuit }
      'i'     { TokenInsert }
      '{'     { TokenStartComposition }
      '}'     { TokenEndComposition }
      text    { TokenText $$ }
      address { TokenAddress $$ }

%%

Cmds : Cmd Cmds { $1 : $2 }
     | Cmd      { [$1] }

Cmd   : addr 'a' text     { AddCmd $1 $3 }
      | addr 'i' text     { InsertCmd $1 $3 }
      | addr '{' Cmds '}' { ComposedCmd $1 $3 }
      | 'q'               { QuitCmd }

addr :         { DotAddress }
     | address { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Cmd
  = AddCmd Address
           String
  | InsertCmd Address
              String
  | QuitCmd
  | ComposedCmd Address
                [Cmd]
  deriving (Show)

data Address
  = DotAddress
  | RegexAddress String
  | OffsetAddress Int
  | LineAddress Int
  | ComposedAddress Address
                    Address
  deriving (Show)

data Token
  = TokenAdd
  | TokenQuit
  | TokenInsert
  | TokenStartComposition
  | TokenEndComposition
  | TokenText String
  | TokenAddress Address
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)
  | isSpace c = lexer cs
  | isNumber c = lexAddress input
lexer ('a':cs) = TokenAdd : lexText cs
lexer ('i':cs) = TokenInsert : lexText cs
lexer ('q':cs) = TokenQuit : lexer cs
lexer ('{':cs) = TokenStartComposition : lexer cs
lexer ('}':cs) = TokenEndComposition : lexer cs
lexer s = error $ "cant parse: " ++ show s

lexAddress :: String -> [Token]
lexAddress cs =
  case span isNumber cs of
    (num, rest) -> TokenAddress (LineAddress $ read num) : lexer rest

lexText :: String -> [Token]
lexText (c:cs)
  | c == '\n' =  case readTextBlock cs
    of (text, rest) -> TokenText text : lexer rest
  | isSpace c = lexText cs
  | isAlphaNum c = error "missing separator"
  | otherwise = lexTextLine c cs

lexTextLine :: Char -> String -> [Token]
lexTextLine separator cs =
  case span (\s -> s /= separator && s /= '\n') cs of
    (text, rest) -> TokenText text : lexer (tail rest)

readTextBlock :: String -> (String, String)
readTextBlock cs =
  case span (/= '\n') cs of
    (".", rest) -> ([], rest)
    (line, rest) -> case readTextBlock (tail rest) of
      (text, r) -> (line ++ '\n':text, r) -- TODO use string appender from LYAH to improve performance

}
