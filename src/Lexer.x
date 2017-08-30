{
module Lexer
    ( Token (..)
    , scan
    ) where

import Graphics.GL.Types
}

%wrapper "basic"

$digit = [0-9]
$char = [a-z]

tokens :-
    $white+               ;
    \#                    ;
    \-?$digit* \. $digit+ { \s -> TkFloating (read s)  }
    \-?[0-9][$digit]*     { \s -> TkInt ((read s) - 1) }
    \/                    { \s -> TkSlash              }
    "v"                   { \s -> TkV                  }
    "vt"                  { \s -> TkVT                 }
    "vn"                  { \s -> TkVN                 }
    "f"                   { \s -> TkF                  }
    $char+                ;
    \.                    ;
{

data Token
    = TkFloating GLfloat
    | TkV
    | TkF
    | TkVN
    | TkVT
    | TkInt GLuint
    | TkSlash
    deriving (Eq, Show)

scan = alexScanTokens

}
