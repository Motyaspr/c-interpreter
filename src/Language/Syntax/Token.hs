module Language.Syntax.Token where

import           Language.Syntax.Internals (ToSourceCode (..))

data Token = Token TokenPosition TokenClass
    deriving (Eq, Show)

type TokenPosition = (Int, Int)

data TokenClass = TokenIf
    | TokenElse
    | TokenFor
    | TokenWhile
    | TokenReturn
    | TokenComma
    | TokenSemicolon
    | TokenOpenRoundBracket
    | TokenCloseRoundBracket
    | TokenOpenCurlyBracket
    | TokenCloseCurlyBracket
    | TokenPlus
    | TokenMinus
    | TokenMultiply
    | TokenDivide
    | TokenAnd
    | TokenOr
    | TokenNot
    | TokenEq
    | TokenNotEq
    | TokenGreater
    | TokenLess
    | TokenGreaterOrEq
    | TokenLessOrEq
    | TokenAssignment
    | TokenTypeDeclaration TokenType
    | TokenIndentifier String
    | TokenString String
    | TokenInt Int
    | TokenFloat Float
    | TokenBool Bool
    | TokenEOF
    deriving (Eq, Show)

data TokenType = TokenTypeInt
    | TokenTypeFloat
    | TokenTypeBool
    | TokenTypeString
    deriving (Eq, Show)

instance ToSourceCode Token where
  toSourceCode (Token _ tc) = toSourceCode tc

instance ToSourceCode TokenClass where
  toSourceCode TokenIf                  = "if"
  toSourceCode TokenElse                = "else"
  toSourceCode TokenFor                 = "for"
  toSourceCode TokenWhile               = "while"
  toSourceCode TokenReturn              = "return"
  toSourceCode TokenComma               = ","
  toSourceCode TokenSemicolon           = ";"
  toSourceCode TokenOpenRoundBracket    = "("
  toSourceCode TokenCloseRoundBracket   = ")"
  toSourceCode TokenOpenCurlyBracket    = "{"
  toSourceCode TokenCloseCurlyBracket   = "}"
  toSourceCode TokenPlus                = "+"
  toSourceCode TokenMinus               = "-"
  toSourceCode TokenMultiply            = "*"
  toSourceCode TokenDivide              = "/"
  toSourceCode TokenAnd                 = "&&"
  toSourceCode TokenOr                  = "||"
  toSourceCode TokenNot                 = "!"
  toSourceCode TokenEq                  = "=="
  toSourceCode TokenNotEq               = "!="
  toSourceCode TokenGreater             = ">"
  toSourceCode TokenLess                = "<"
  toSourceCode TokenGreaterOrEq         = ">="
  toSourceCode TokenLessOrEq            = "<="
  toSourceCode TokenAssignment          = "=="
  toSourceCode (TokenTypeDeclaration t) = toSourceCode t
  toSourceCode (TokenIndentifier s)     = s
  toSourceCode (TokenString s)          = "\"" <> s <> "\""
  toSourceCode (TokenInt x)             = show x
  toSourceCode (TokenFloat x)           = show x
  toSourceCode (TokenBool x)            = show x
  toSourceCode TokenEOF                 = ""

instance ToSourceCode TokenType where
  toSourceCode TokenTypeInt    = "int"
  toSourceCode TokenTypeFloat  = "float"
  toSourceCode TokenTypeBool   = "bool"
  toSourceCode TokenTypeString = "string"
