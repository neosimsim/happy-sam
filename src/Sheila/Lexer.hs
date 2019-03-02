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
  = TokenCmd String
  | TokenEnd
  | TokenDot
  | TokenPlus
  | TokenMinute
  | TokenComma
  | TokenSemicolon
  | TokenZeroComposition
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
  | TokenSubstitution (String, String)
  deriving (Show)

data LexingMode
  = CommandMode
  | TextMode
  | FooMode
  | Foo2Mode
  | RegexMode -- ^ lex @/regexp/@, with @/@ is a non alpha numeric.
  | StringMode -- ^ lex everything till end of line
  | SubstituteMode -- ^ lex @/regexp/string/@
  deriving (Show)

type P a
   = String -> LexingMode -> String -> Int -- ^ depth of command composition
                                        -> Either String (a, String)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k =
  \s mode rest level ->
    case m s mode rest level of
      Right (x, r') -> k x s mode r' level
      Left err      -> Left err

returnP :: a -> P a
returnP a _ _ r _ = Right (a, r)

parseError :: Token -> P a
parseError (TokenCmd c) _ _ _ _
  | c `elem` ["b", "B", "n", "D", "e", "f", "!", "cd", "X", "Y", "q", "u"] =
    Left "command takes no address"
  | otherwise = Left $ "unexpected " ++ c
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
    -- Display Commands
    ('p':cs) -> cont (TokenCmd "p") cs CommandMode cs
    ('=':'#':cs) -> cont (TokenCmd "=#") cs CommandMode cs
    ('=':cs) -> cont (TokenCmd "=") cs CommandMode cs
    -- File Commands
    ('b':cs) -> cont (TokenCmd "b") cs StringMode cs
    ('B':cs) -> cont (TokenCmd "B") cs StringMode cs
    ('n':cs) -> cont (TokenCmd "n") cs CommandMode cs
    ('D':cs) -> cont (TokenCmd "D") cs StringMode cs
    -- I/O Commands
    ('e':cs) -> cont (TokenCmd "e") cs StringMode cs
    ('r':cs) -> cont (TokenCmd "r") cs StringMode cs
    ('w':cs) -> cont (TokenCmd "w") cs StringMode cs
    ('f':cs) -> cont (TokenCmd "f") cs StringMode cs
    ('<':cs) -> cont (TokenCmd "<") cs StringMode cs
    ('>':cs) -> cont (TokenCmd ">") cs StringMode cs
    ('|':cs) -> cont (TokenCmd "|") cs StringMode cs
    ('!':cs) -> cont (TokenCmd "!") cs StringMode cs
    ('c':'d':cs) -> cont (TokenCmd "cd") cs StringMode cs
    -- Text Commands
    ('a':cs) -> cont (TokenCmd "a") cs TextMode cs
    ('c':cs) -> cont (TokenCmd "c") cs TextMode cs
    ('i':cs) -> cont (TokenCmd "i") cs TextMode cs
    ('d':cs) -> cont (TokenCmd "d") cs CommandMode cs
    ('s':cs) -> cont (TokenCmd "s") cs SubstituteMode cs
    ('m':cs) -> cont (TokenCmd "m") cs CommandMode cs
    ('t':cs) -> cont (TokenCmd "t") cs CommandMode cs
    -- Loop, Conditionals and Groups
    ('x':cs) -> cont (TokenCmd "x") cs FooMode cs
    ('y':cs) -> cont (TokenCmd "y") cs FooMode cs
    ('X':cs) -> cont (TokenCmd "X") cs Foo2Mode cs
    ('Y':cs) -> cont (TokenCmd "Y") cs Foo2Mode cs
    ('g':cs) -> cont (TokenCmd "g") cs TextMode cs
    ('v':cs) -> cont (TokenCmd "v") cs TextMode cs
    ('{':cs) ->
      \level -> cont TokenZeroComposition cs CommandMode cs (level + 1)
    ('}':cs) -> \level -> cont TokenEndComposition cs CommandMode cs (level - 1)
    -- misc
    ('q':cs) -> cont (TokenCmd "q") cs CommandMode cs
    ('k':cs) -> cont (TokenCmd "k") cs CommandMode cs
    ('u':cs) -> cont (TokenCmd "u") cs CommandMode cs
    -- Addresses
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
      | isNumber c -> lexNumber cont input CommandMode input
      | otherwise -> const . Left $ "invalid command line: " ++ s
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
lexer cont s RegexMode _ =
  case s of
    (c:cs)
      | isSpace c -> lexer cont cs RegexMode cs
      | not (isAlphaNum c) -> lexTextLine c cont cs CommandMode cs
      | otherwise -> const . Left $ "invalid text " ++ show s
    [] -> const $ Left "unexpected eof"
lexer cont s StringMode _ =
  case s of
    (c:cs)
      | c == '\n' -> cont (TokenText "") s CommandMode s
      | isSpace c -> lexer cont cs StringMode cs
      | otherwise ->
        case span (/= '\n') s of
          (line, rest) -> cont (TokenText line) rest CommandMode rest
    [] -> const $ Left "unexpected eof"
lexer cont s SubstituteMode _ =
  case s of
    (c:cs)
      | isSpace c -> lexer cont cs SubstituteMode cs
      | not (isAlphaNum c) -> lexTouple c cont cs CommandMode cs
      | otherwise -> const . Left $ "invalid substitution " ++ show s
    [] -> const $ Left "unexpected eof"
lexer cont s FooMode _ =
  case s of
    (c:cs)
      | c == '\n' -> cont (TokenText ".*\\n") s CommandMode s
      | isSpace c -> lexer cont cs FooMode cs
      | isAlphaNum c -> cont (TokenText ".*\\n") s CommandMode s
      | otherwise -> lexTextLine c cont cs CommandMode cs
    [] -> const $ Left "unexpected eof"
lexer cont s Foo2Mode _ =
  case s of
    (c:cs)
      | c == '\n' -> cont (TokenText "") s CommandMode s
      | isSpace c -> lexer cont cs Foo2Mode cs
      | isAlphaNum c -> cont (TokenText "") s CommandMode s
      | otherwise -> lexTextLine c cont cs CommandMode cs
    [] -> const $ Left "unexpected eof"

lexNumber :: (Token -> P a) -> P a
lexNumber cont cs _ _ =
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

lexTouple :: Char -> (Token -> P a) -> P a
lexTouple separator cont cs mode _ =
  case unescape separator cs of
    (regexp, rest) ->
      case unescape separator rest of
        (text, rest2) ->
          cont (TokenSubstitution (regexp, text)) rest2 mode rest2

lexTextLine :: Char -> (Token -> P a) -> P a
lexTextLine separator cont cs mode _ =
  case unescape separator cs of
    (text, rest) -> cont (TokenText text) rest mode rest

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

readTextBlock :: String -> (String, String)
readTextBlock cs =
  case span (/= '\n') cs of
    (".", rest) -> ([], rest)
    (line, rest) ->
      case readTextBlock (tail rest) of
        (text, r) -> (line ++ '\n' : text, r) -- TODO use string appender from LYAH to improve performance
