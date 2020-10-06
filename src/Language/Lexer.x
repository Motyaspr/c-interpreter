{

module Language.Lexer (scanTokens) where

import Language.Syntax.Token

}

%wrapper "posn"

$digit = 0-9
$letter = [a-zA-Z]

@string = \" [^\"]* \"
@float = $digit+ [ \. ] $digit*
@int = $digit+
$eol = [\n]

tokens :-

  $eol ;
  $white ;
  "//".* ;

  int { tok (TokenTypeDeclaration TokenTypeInt) }
  float { tok (TokenTypeDeclaration TokenTypeFloat) }
  bool { tok (TokenTypeDeclaration TokenTypeBool) }
  string { tok (TokenTypeDeclaration TokenTypeString) }

  if { tok TokenIf }
  else { tok TokenElse }
  for { tok TokenFor }
  while { tok TokenWhile }
  return { tok TokenReturn }

  \, { tok TokenComma }
  \; { tok TokenSemicolon }
  \( { tok TokenOpenRoundBracket }
  \) { tok TokenCloseRoundBracket }
  \{ { tok TokenOpenCurlyBracket }
  \} { tok TokenCloseCurlyBracket }

  \+ { tok TokenPlus }
  \- { tok TokenMinus }
  \* { tok TokenMultiply }
  \/ { tok TokenDivide }

  "&&" { tok TokenAnd }
  "||" { tok TokenOr }
  \! { tok TokenNot }

  "==" { tok TokenEq }
  "!=" { tok TokenNotEq }
  \> { tok TokenGreater }
  \< { tok TokenLess }
  ">=" { tok TokenGreaterOrEq  }
  "<=" { tok TokenLessOrEq  }

  \= { tok TokenAssignment }


  @float { tokValue TokenTypeFloat }
  @int { tokValue TokenTypeInt }

  0 { tokValue TokenTypeBool }
  1 { tokValue TokenTypeBool }

  @string { tokValue TokenTypeString }

  $letter [$letter $digit \_]* { tokIdentifier }

{

tok :: TokenClass -> AlexPosn -> String -> Token
tok t p _ = Token (posnToPair p) t

posnToPair :: AlexPosn -> (Int, Int)
posnToPair (AlexPn _ x y) = (x, y)

tokValue :: TokenType -> AlexPosn -> String -> Token
tokValue TokenTypeString p s = Token (posnToPair p) (TokenString s)
tokValue TokenTypeInt p s = Token (posnToPair p) (TokenInt (read s))
tokValue TokenTypeFloat p s = Token (posnToPair p) (TokenFloat (read s))
tokValue TokenTypeBool p s
  | s == "0" = Token (posnToPair p) (TokenBool False)
  | otherwise = Token (posnToPair p) (TokenBool True)

tokIdentifier :: AlexPosn -> String -> Token
tokIdentifier p s = Token (posnToPair p) (TokenIndentifier s)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}
