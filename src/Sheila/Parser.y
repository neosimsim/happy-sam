{
module Sheila.Parser
  ( parseCommand
  ) where

import           Data.Char
import           Sheila.Types
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
      '"'  { TokenFileAddressSeparator }
      '#'  { TokenOffset }
      '/'  { TokenRegexpSeparator }
      '?'  { TokenReverseRegexpSeparator }
      '.'  { TokenDot }
      '+'  { TokenPlus }
      '-'  { TokenMinute }
      ','  { TokenComma }
      ';'  { TokenSemicolon }
      '$'  { TokenEnd }
      text { TokenText $$ }
      number { TokenNumber $$ }

%left ',' ';'
%left '+' '-'
%nonassoc '#' '?' '/' '"' '$' number
%%

Cmd   : address                        { PrintCmd $1 }
      | address 'a' text               { AddCmd $1 $3 }
      | address 'i' text               { InsertCmd $1 $3 }
      | address 'g' text Cmd           { InsertCmd $1 $3 }
      | address '{' '\n' Cmds '\n' '}' { ComposedCmd $1 (reverse $4) }
      | 'q'                            { QuitCmd }

Cmds : Cmds '\n' Cmd { $3 : $1 }
     | Cmd           { [$1] }

address: {- empty -}        { DotAddress }
       | '.'                { DotAddress }
       | composedAddress    { $1 }

composedAddress: simpleAddress                       { $1 }
               | composedAddress simpleAddress       { PlusAddress $1 $2 }
               | composedAddress '+' composedAddress { PlusAddress $1 $3 }
               | composedAddress '+'                 { PlusAddress $1 (LineAddress 1) }
               |                 '+' composedAddress { PlusAddress DotAddress $2 }
               |                 '+'                 { PlusAddress DotAddress (LineAddress 1) }
               | composedAddress '-' composedAddress { MinusAddress $1 $3 }
               | composedAddress '-'                 { MinusAddress $1 (LineAddress 1) }
               |                 '-' composedAddress { MinusAddress DotAddress $2 }
               |                 '-'                 { MinusAddress DotAddress (LineAddress 1) }
               | composedAddress ',' composedAddress { RangeAddress $1 $3 }
               |                 ',' composedAddress { RangeAddress BeginAddress $2 }
               | composedAddress ','                 { RangeAddress $1 EndAddress }
               |                 ','                 { RangeAddress BeginAddress EndAddress }
               | composedAddress ';' composedAddress { relRangeAddr $1 $3 }
               |                 ';' composedAddress { relRangeAddr BeginAddress $2 }
               | composedAddress ';'                 { relRangeAddr $1 EndAddress }
               |                 ';'                 { relRangeAddr BeginAddress EndAddress }

simpleAddress: number           { if $1 == 0 then BeginAddress else LineAddress $1 }
             | '#' number       { OffsetAddress $2 }
             | '/' text         { RegexpAddress $2 }
             | '?' text         { MinusAddress DotAddress (RegexpAddress $2) }
             | '$'              { EndAddress }
             | '"' text address { FileAddress $2 $3 }

{
relRangeAddr r1 r2 = RangeAddress r1 (PlusAddress r1 r2)

data Token
  = TokenAdd
  | TokenQuit
  | TokenInsert
  | TokenEnd
  | TokenDot
  | TokenPlus
  | TokenMinute
  | TokenComma
  | TokenSemicolon
  | TokenStartComposition
  | TokenEndComposition
  | TokenNewLine
  | TokenEOF
  | TokenText String
  | TokenAddress Address
  | TokenFileAddressSeparator
  | TokenOffset
  | TokenNumber Int
  | TokenRegexpSeparator
  | TokenReverseRegexpSeparator
  deriving (Show)

data LexingMode = CommandMode | TextMode deriving (Show)

type P a = String -> LexingMode -> String -> Int -> Either String (a, String)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s mode rest level ->
   case m s mode rest level of
       Right (x, r') -> k x s mode r' level
       Left err -> Left err

returnP :: a -> P a
returnP a = \s _ r _ -> Right (a, r)

parseError :: Token -> P a
parseError t s m r _ = Left $ "Parse error: string: " ++ show s ++ ", token: " ++ show t ++ ", mode: " ++ show m ++ ", rest: " ++ show r

lexer :: (Token -> P a) -> P a
lexer cont s CommandMode r = case s of
  [] -> cont TokenEOF [] CommandMode r
  ('\n':cs) -> \level -> case level of -- EOF unless we're nested
    0 -> cont TokenEOF cs CommandMode cs level
    otherwise -> cont TokenNewLine cs CommandMode cs level
  ('a':cs) -> cont TokenAdd cs TextMode cs
  ('i':cs) -> cont TokenInsert cs TextMode cs
  ('q':cs) -> cont TokenQuit cs CommandMode cs
  ('{':cs) -> \level -> cont TokenStartComposition cs CommandMode cs (level+1)
  ('}':cs) -> \level -> cont TokenEndComposition cs CommandMode cs (level-1)
  ('#':cs) -> cont TokenOffset cs CommandMode cs
  ('.':cs) -> cont TokenDot cs CommandMode cs
  ('"':cs) -> cont TokenFileAddressSeparator s TextMode s
  ('/':cs) -> cont TokenRegexpSeparator s TextMode s
  ('?':cs) -> cont TokenReverseRegexpSeparator s TextMode s
  ('+':cs) -> cont TokenPlus cs CommandMode cs
  ('-':cs) -> cont TokenMinute cs CommandMode cs
  (',':cs) -> cont TokenComma cs CommandMode cs
  (';':cs) -> cont TokenSemicolon cs CommandMode cs
  ('$':cs) -> cont TokenEnd cs CommandMode cs
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
    (num, rest) -> cont (TokenNumber $ read num) rest CommandMode rest

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
