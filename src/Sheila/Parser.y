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
      'a'  { TokenAdd }
      'g'  { TokenAdd }
      'q'  { TokenQuit }
      'i'  { TokenInsert }
      '{'  { TokenStartComposition }
      '}'  { TokenEndComposition }
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

%left ',' ';'
%left '+' '-'
%nonassoc '#' '"' '$' number regexp backwardsRegexp
%%

Cmd   : address                        { PrintCmd $1 }
      | address 'a' text               { AddCmd $1 $3 }
      | address 'i' text               { InsertCmd $1 $3 }
      | address 'g' text Cmd           { InsertCmd $1 $3 }
      | address '{' '\n' Cmds '\n' '}' { ComposedCmd $1 (reverse $4) }
      | 'q'                            { QuitCmd }

Cmds : Cmds '\n' Cmd { $3 : $1 }
     | Cmd           { [$1] }

address: {- empty -}      { DotAddress }
       | '.'              { DotAddress }
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
               |                 ',' composedAddress { RangeAddress BeginAddress $2 }
               | composedAddress ','                 { RangeAddress $1 EndAddress }
               |                 ','                 { RangeAddress BeginAddress EndAddress }
               | composedAddress ';' composedAddress { relRangeAddr $1 $3 }
               |                 ';' composedAddress { relRangeAddr BeginAddress $2 }
               | composedAddress ';'                 { relRangeAddr $1 EndAddress }
               |                 ';'                 { relRangeAddr BeginAddress EndAddress }

simpleAddress: number           { if $1 == 0 then BeginAddress else LineAddress $1 }
             | '#' number       { OffsetAddress $2 }
             | regexp           { RegexpAddress $1 }
             | backwardsRegexp  { MinusAddress DotAddress (RegexpAddress $1) }
             | '$'              { EndAddress }

{
relRangeAddr r1 r2 = RangeAddress r1 (PlusAddress r1 r2)

parseCommand :: String -> Either String (Cmd, String)
parseCommand s = parse s CommandMode s 0
}
