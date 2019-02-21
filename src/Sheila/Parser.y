{
module Sheila.Parser
  ( parseCommand
  ) where

import           Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

%token
      'a'  { TokenAdd }
      'g'  { TokenAdd }
      'q'  { TokenQuit }
      'i'  { TokenInsert }
      '{'  { TokenStartComposition }
      '}'  { TokenEndComposition }
      '\n' { TokenNewLine }
      text { TokenText $$ }
      addr { TokenAddress $$ }

%%

Cmd   : address                        { PrintCmd $1 }
      | address 'a' text               { AddCmd $1 $3 }
      | address 'i' text               { InsertCmd $1 $3 }
      | address 'g' text Cmd           { InsertCmd $1 $3 }
      | address '{' '\n' Cmds '\n' '}' { ComposedCmd $1 (reverse $4) }
      | 'q'                            { QuitCmd }

Cmds : Cmds '\n' Cmd { $3 : $1 }
     | Cmd      { [$1] }

address : {- empty -} { DotAddress }
        | addr        { $1 }

{
data Cmd
  = PrintCmd Address
  | AddCmd Address
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
  | TokenNewLine
  | TokenEOF
  | TokenText String
  | TokenAddress Address
  deriving (Show)

data LexingMode = CommandMode | TextMode

type P a = String -> LexingMode -> String -> Int -> Either String (a, String)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s mode rest level ->
   case m s mode rest level of
       Right (x, r') -> k x s mode r' level
       Left err -> Left err

returnP :: a -> P a
returnP a = \s _ r _ -> Right (a, r)

parseError :: Token -> P a
parseError t s _ r _ = Left $ "Parse error: " ++ show t ++ show r

lexer :: (Token -> P a) -> P a
lexer cont s CommandMode r = case s of
  [] -> cont TokenEOF [] CommandMode r
  ('\n':cs) -> \level -> case level of
    0 -> cont TokenEOF cs CommandMode cs level
    otherwise -> cont TokenNewLine cs CommandMode cs level
  ('a':cs) -> cont TokenAdd cs TextMode cs
  ('i':cs) -> cont TokenInsert cs TextMode cs
  ('q':cs) -> cont TokenQuit cs CommandMode cs
  ('{':cs) -> \level -> cont TokenStartComposition cs CommandMode cs (level+1)
  ('}':cs) -> \level -> cont TokenEndComposition cs CommandMode cs (level-1)
  input@(c:cs)
    | isSpace c -> lexer cont cs CommandMode cs
    | isNumber c -> lexAddress cont input CommandMode input
    | otherwise -> const . Left $ "invalid command line:\n" ++ s
lexer cont s TextMode _ =
  case s of
    (c:cs)
      | c == '\n' ->
        case readTextBlock cs of
          (text, rest) -> cont (TokenText text) rest CommandMode rest
      | isSpace c -> lexer cont cs TextMode cs
      | not (isAlphaNum c) -> lexTextLine c cont cs CommandMode cs
      | otherwise -> const . Left $ "invalid text " ++ show s

lexAddress :: (Token -> P a) -> P a
lexAddress cont cs _ _ =
  case span isNumber cs of
    (num, rest) -> cont (TokenAddress (LineAddress $ read num)) rest CommandMode rest

lexTextLine :: Char -> (Token -> P a) -> P a
lexTextLine separator cont cs _ _ =
  case span (\s -> s /= separator && s /= '\n') cs of
    (text, rest) -> case rest of
      ('\n':_) -> cont (TokenText text) rest CommandMode rest
      (_:r) -> cont (TokenText text) r CommandMode r

readTextBlock :: String -> (String, String)
readTextBlock cs =
  case span (/= '\n') cs of
    (".", rest) -> ([], rest)
    (line, rest) -> case readTextBlock (tail rest) of
      (text, r) -> (line ++ '\n':text, r) -- TODO use string appender from LYAH to improve performance

parseCommand :: String -> Either String (Cmd, String)
parseCommand s = parse s CommandMode s 0
}
