{
module Sheila.Parser
  ( parseCommand
  ) where

import           Data.Char
import           Sheila.Types
import           Sheila.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

%token
      -- Text Commands
      'a'  { TokenCmd "a" }
      'c'  { TokenCmd "c" }
      'i'  { TokenCmd "i" }
      'd'  { TokenCmd "d" }
      's'  { TokenCmd "s" }
      'm'  { TokenCmd "m" }
      't'  { TokenCmd "t" }
      -- Display Command
      'p'  { TokenCmd "p" }
      '='  { TokenCmd "=" }
      '=#' { TokenCmd "=#" }
      -- File Commands
      'b'  { TokenCmd "b" }
      'B'  { TokenCmd "B" }
      'n'  { TokenCmd "n" }
      'D'  { TokenCmd "D" }
      -- I/O Commands
      'e'  { TokenCmd "e" }
      'r'  { TokenCmd "r" }
      'w'  { TokenCmd "w" }
      'f'  { TokenCmd "f" }
      '<'  { TokenCmd "<" }
      '>'  { TokenCmd ">" }
      '|'  { TokenCmd "|" }
      '!'  { TokenCmd "!" }
      "cd"  { TokenCmd "cd" }
      -- Loops and Conditionals
      'x'  { TokenCmd "x" }
      'y'  { TokenCmd "y" }
      'X'  { TokenCmd "X" }
      'Y'  { TokenCmd "Y" }
      'g'  { TokenCmd "g" }
      'v'  { TokenCmd "v" }
      '{'  { TokenZeroComposition }
      '}'  { TokenEndComposition }
      -- Misc
      'q'  { TokenCmd "q" }
      'k'  { TokenCmd "k" }
      'u'  { TokenCmd "u" }
      '\n' { TokenNewLine }
      '"'  { TokenFileAddressSeparator }
      '#'  { TokenOffset }
      regexp  { TokenRegexp  $$ }
      backwardsRegexp  { TokenBackwardsRegexp $$ }
      '.'  { TokenDot }
      '+'  { TokenPlus }
      '-'  { TokenMinute }
      ','  { TokenComma }
      ';'  { TokenSemicolon }
      '$'  { TokenEnd }
      text { TokenText $$ }
      number { TokenNumber $$ }
      substitution {TokenSubstitution $$ }

%left ',' ';'
%left '+' '-'
%nonassoc '#' '"' '$' number regexp backwardsRegexp
%%

Line : {- empty -} { Print (PlusAddress DotAddress (LineAddress 1)) }
     | Cmd         { $1 }

Cmd   : address                        { Print $1 }
      | optionalAddress 'a' text               { Add $1 $3 }
      | optionalAddress 'i' text               { Insert $1 $3 }
      | optionalAddress 'c' text               { Change $1 $3 }
      | optionalAddress 'd'                    { Delete $1 }
      | optionalAddress 'k'                    { Mark $1 }
      | 'u' number                     { Undo $2 }
      | 'u'                            { Undo 1 }
      | optionalAddress 's' substitution       { Substitute $1 (fst $3) (snd $3) }
      | optionalAddress 'm' address            { Move $1 $3 }
      | optionalAddress 't' address            { Copy $1 $3 }
      | optionalAddress 'g' text Cmd           { RunIf $1 $3 $4 }
      | optionalAddress 'v' text Cmd           { RunIfNot $1 $3 $4 }
      | optionalAddress 'x' text Cmd           { LoopIf $1 $3 $4 }
      | optionalAddress 'x' text               { LoopIf $1 $3 (Print DotAddress) }
      | optionalAddress 'y' text Cmd           { LoopIfNot $1 $3 $4 }
      | optionalAddress 'y' text               { LoopIfNot $1 $3 (Print DotAddress) }
      | optionalAddress '{' '\n' Cmds '\n' '}' { Composed $1 (reverse $4) }
      | 'e' restOfLine                 { Edit $2 }
      | optionalAddress 'r' restOfLine         { Replace $1 $3 }
      | address 'w' restOfLine         { Write $1 $3 } -- TODO DotAddress is not the default here
      | 'w' restOfLine         { Write (RangeAddress ZeroAddress EndAddress) $2 } -- TODO DotAddress is not the default here
      | 'f' word                       { SetFilename $2 }
      | optionalAddress '<' restOfLine         { PipeIn $1 $3 }
      | optionalAddress '>' restOfLine         { PipeOut $1 $3 }
      | optionalAddress '|' restOfLine         { Pipe $1 $3 }
      |  '!' restOfLine                { RunShell $2 }
      |  "cd" restOfLine               { ChangeDir $2 }
      | 'X' text Cmd                   { LoopIfFile $2 $3 }
      | 'Y' text Cmd                   { LoopIfNotFile $2 $3 }
      | 'q'                            { Quit }
      | 'b' restOfLine                 { SetBuffer (words $2) } -- TODO should use @words@ here
      | 'B' restOfLine                 { AddBuffer (words $2) }
      | 'n'                            { PrintMenu }
      | 'D' restOfLine                 { DeleteFiles (words $2) }
      | optionalAddress 'p'                    { Print $1 }
      | optionalAddress '='                    { PrintRange $1 }
      | optionalAddress '=#'                   { PrintOffset $1 }

restOfLine: text { $1 }

word: text { $1 }

Cmds : Cmds '\n' Line { $3 : $1 }
     | Line           { [$1] }

optionalAddress: {- empty -} { DotAddress }
               | address     { $1 }

address: '.'              { DotAddress }
       | composedAddress  { $1 }
       | '"' text address { FileAddress $2 $3 }

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
               |                 ',' composedAddress { RangeAddress ZeroAddress $2 }
               | composedAddress ','                 { RangeAddress $1 EndAddress }
               |                 ','                 { RangeAddress ZeroAddress EndAddress }
               | composedAddress ';' composedAddress { relRangeAddr $1 $3 }
               |                 ';' composedAddress { relRangeAddr ZeroAddress $2 }
               | composedAddress ';'                 { relRangeAddr $1 EndAddress }
               |                 ';'                 { relRangeAddr ZeroAddress EndAddress }

simpleAddress: number           { if $1 == 0 then ZeroAddress else LineAddress $1 }
             | '#' number       { OffsetAddress $2 }
             | regexp           { RegexpAddress $1 }
             | backwardsRegexp  { MinusAddress DotAddress (RegexpAddress $1) }
             | '$'              { EndAddress }

{
relRangeAddr r1 r2 = RangeAddress r1 (PlusAddress r1 r2)

-- | Parses the next command from the beginning of the string.
-- Returs a touple of the parsed command and the unparsed rest of the string.
parseCommand :: String -> Either String (Cmd, String)
parseCommand s = parse s CommandMode s 0
}
