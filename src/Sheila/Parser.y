{
module Sheila.Parser (parseFoo, lexer) where

import Data.Char
}

%partial parseFoo
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

%token
      'a'     { TokenAdd }
      'q'     { TokenQuit }
      'i'     { TokenInsert }
      '{'     { TokenStartComposition }
      '}'     { TokenEndComposition }
      text    { TokenText $$ }
      address { TokenAddress $$ }

%%

Cmd   : addr 'a' text     { AddCmd $1 $3 }
      | addr 'i' text     { InsertCmd $1 $3 }
      | addr '{' Cmds '}' { ComposedCmd $1 $3 }
      | 'q'               { QuitCmd }

Cmds : Cmds Cmd { $2 : $1 }
     | Cmd      { [$1] }

addr : {- empty -} { DotAddress }
     | address     { $1 }

{
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
  | TokenEOF
  | TokenText String
  | TokenAddress Address
  deriving (Show)

type P a = String -> Either String a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
   case m s of
       Right x -> k x s
       Left err -> Left err

returnP :: a -> P a
returnP a = \s -> Right a

parseError :: Token -> P a
parseError t s = Left $ "Parse error: " ++ show t ++ show s

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
  [] -> cont TokenEOF []
  ('\n':cs) -> case readTextBlock cs
    of (text, rest) -> cont (TokenText text) rest
  input@(c:cs)
    | isSpace c -> lexer cont cs
    | isNumber c -> lexAddress cont input
    | not (isAlpha c) -> lexTextLine c cont cs
  ('a':cs) -> cont TokenAdd cs
  ('i':cs) -> cont TokenInsert cs
  ('q':cs) -> cont TokenQuit cs
  ('{':cs) -> cont TokenStartComposition cs
  ('}':cs) -> cont TokenEndComposition cs

lexAddress :: (Token -> P a) -> P a
lexAddress cont cs =
  case span isNumber cs of
    (num, rest) -> cont (TokenAddress (LineAddress $ read num)) rest

lexText :: (Token -> P a) -> P a
lexText cont (c:cs)
  | c == '\n' =  case readTextBlock cs
    of (text, rest) -> cont (TokenText text) rest
  | isSpace c = lexText cont cs
  | isAlphaNum c = error "missing separator"
  | otherwise = lexTextLine c cont cs

lexTextLine :: Char -> (Token -> P a) -> P a
lexTextLine separator cont cs =
  case span (\s -> s /= separator && s /= '\n') cs of
    (text, rest) -> cont (TokenText text) (tail rest)

readTextBlock :: String -> (String, String)
readTextBlock cs =
  case span (/= '\n') cs of
    (".", rest) -> ([], rest)
    (line, rest) -> case readTextBlock (tail rest) of
      (text, r) -> (line ++ '\n':text, r) -- TODO use string appender from LYAH to improve performance

}
