module Sheila.Lexer
  ( lexer
  , P
  , returnP
  , thenP
  , parseError
  , Token(..)
  , LexingMode(..)
  ) where

import           Data.Char
import           Sheila.Types

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
  | TokenRegexp String
  | TokenBackwardsRegexp String
  deriving (Show)

data LexingMode
  = CommandMode
  | TextMode
  deriving (Show)

type P a = String -> LexingMode -> String -> Int -> Either String (a, String)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k =
  \s mode rest level ->
    case m s mode rest level of
      Right (x, r') -> k x s mode r' level
      Left err      -> Left err

returnP :: a -> P a
returnP a _ _ r _ = Right (a, r)

parseError :: Token -> P a
parseError t s m r _ =
  Left $
  "Parse error: string: " ++
  show s ++
  ", token: " ++ show t ++ ", mode: " ++ show m ++ ", rest: " ++ show r

lexer :: (Token -> P a) -> P a
lexer cont s CommandMode r =
  case s of
    [] -> cont TokenEOF [] CommandMode r
    ('\n':cs) ->
      \level ->
        case level -- EOF unless we're nested
              of
          0 -> cont TokenEOF cs CommandMode cs level
          _ -> cont TokenNewLine cs CommandMode cs level
    ('a':cs) -> cont TokenAdd cs TextMode cs
    ('i':cs) -> cont TokenInsert cs TextMode cs
    ('q':cs) -> cont TokenQuit cs CommandMode cs
    ('{':cs) ->
      \level -> cont TokenStartComposition cs CommandMode cs (level + 1)
    ('}':cs) -> \level -> cont TokenEndComposition cs CommandMode cs (level - 1)
    ('#':cs) -> cont TokenOffset cs CommandMode cs
    ('.':cs) -> cont TokenDot cs CommandMode cs
    ('"':_) -> cont TokenFileAddressSeparator s TextMode s
    ('/':cs) -> lexRexexp '/' cont cs CommandMode cs
    ('?':cs) -> lexBackwardsRexexp '?' cont cs CommandMode cs
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
    [] -> const $ Left "unexpected eof"

lexAddress :: (Token -> P a) -> P a
lexAddress cont cs _ _ =
  case span isNumber cs of
    (num, rest) -> cont (TokenNumber $ read num) rest CommandMode rest

lexRexexp :: Char -> (Token -> P a) -> P a
lexRexexp separator cont cs mode _ =
  case unescape separator cs of
    (text, rest) -> cont (TokenRegexp text) rest mode rest

lexBackwardsRexexp :: Char -> (Token -> P a) -> P a
lexBackwardsRexexp separator cont cs mode _ =
  case unescape separator cs of
    (text, rest) -> cont (TokenBackwardsRegexp text) rest mode rest

-- | parse text until separator or '\n'
-- substitutes "\n" by '\n' and "\sep" by 'sep'
unescape :: Char -> String -> (String, String)
unescape _ [] = ([], [])
unescape c (s:ss)
  | s == '\n' = ([], s : ss)
  | s == c = ([], ss)
  | s == '\\' = escaped c ss
  | otherwise = first (s :) (unescape c ss)

escaped :: Char -> String -> (String, String)
escaped _ [] = ("/", [])
escaped c (s:ss)
  | s == '\\' = first ('\\' :) (unescape c ss)
  | s == 'n' = first ('\n' :) (unescape c ss)
  | s == c = first (c :) (unescape c ss)
  | otherwise = first ('\\' :) $ first (s :) (unescape c ss)

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

lexTextLine :: Char -> (Token -> P a) -> P a
lexTextLine separator cont cs mode _ =
  case unescape separator cs of
    (text, rest) -> cont (TokenText text) rest mode rest

readTextBlock :: String -> (String, String)
readTextBlock cs =
  case span (/= '\n') cs of
    (".", rest) -> ([], rest)
    (line, rest) ->
      case readTextBlock (tail rest) of
        (text, r) -> (line ++ '\n' : text, r) -- TODO use string appender from LYAH to improve performance
