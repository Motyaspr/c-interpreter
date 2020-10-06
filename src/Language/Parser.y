{

module Language.Parser where

import Language.Syntax.Token
import Language.Syntax.AST
import Language.Syntax.Internals

}

%name parseTokens
%tokentype { Token }

%monad { Either String } { (>>=) } { return }
%error { parseError }

%token
  if { Token _ TokenIf }
  else { Token _ TokenElse }
  for { Token _ TokenFor }
  while { Token _ TokenWhile }
  return { Token _ TokenReturn }

  "," { Token _ TokenComma }
  ";" { Token _ TokenSemicolon }
  "(" { Token _ TokenOpenRoundBracket  }
  ")" { Token _ TokenCloseRoundBracket  }
  "{" { Token _ TokenOpenCurlyBracket  }
  "}" { Token _ TokenCloseCurlyBracket  }

  "+" { Token _ TokenPlus }
  "-" { Token _ TokenMinus }
  "*" { Token _ TokenMultiply }
  "/" { Token _ TokenDivide }

  "&&" { Token _ TokenAnd }
  "||" { Token _ TokenOr }
  "!" { Token _ TokenNot }

  "==" { Token _ TokenEq }
  "!=" { Token _ TokenNotEq }
  ">" { Token _ TokenGreater }
  "<" { Token _ TokenLess }
  ">=" { Token _ TokenGreaterOrEq }
  "<=" { Token _ TokenLessOrEq }

  "=" { Token _ TokenAssignment }

  int { Token _ (TokenTypeDeclaration TokenTypeInt) }
  float { Token _ (TokenTypeDeclaration TokenTypeFloat) }
  bool { Token _ (TokenTypeDeclaration TokenTypeBool) }
  string { Token _ (TokenTypeDeclaration TokenTypeString) }

  name { Token _ (TokenIndentifier $$) }

  intVal { Token _ (TokenInt $$) }
  floatVal { Token _ (TokenFloat $$) }
  strVal { Token _ (TokenString $$) }
  boolVal { Token _ (TokenBool $$) }

%%

Program : GDecs { AST $1 }

GDecs : GDec { [$1] }
      | GDec GDecs { $1 : $2 }

GDec : Func { FunctionDeclaration $1 }
     | VarDec ";" { GlobalVariableDeclaration $1 }
     | Expr ";" { GlobalExpr $1 }

Func : Type Id "(" FArgs ")" "{" LDecs "}" { Function $2 $1 $4 $7 }

Type : int { TypeInt }
     | float { TypeFloat }
     | bool { TypeBool }
     | string { TypeString }

FArgs : {- empty -} { [] }
      | FArg { [$1] }
      | FArg "," FArgs { $1 : $3 }

FArg : Type Id { Variable $2 $1 Nothing }

VarDec : Type Id "=" Expr { VariableDeclaration (Variable $2 $1 Nothing) (Just $4) }
       | Type Id { VariableDeclaration (Variable $2 $1 Nothing) Nothing }
       | Id "=" Expr { VariableAssignment (VariableUpdate $1 $3) }

Expr : UnaryExpr { Expr $1 }

UnaryExpr : "(" UnaryExpr ")" { $2 }
          | "+" UnaryExpr { UnaryExpr UnaryPlus $2 }
          | "-" UnaryExpr { UnaryExpr UnaryMinus $2 }
          | "!" UnaryExpr { UnaryExpr UnaryNot $2 }
          | MultExpr { UnaryRawExpr $1 }

MultExpr : "(" MultExpr ")" { $2 }
         | AddExpr "*" MultExpr { MultExpr Multiply $1 $3 }
         | AddExpr "/" MultExpr { MultExpr Divide $1 $3 }
         | AddExpr { MultRawExpr $1 }

AddExpr : "(" AddExpr ")" { $2 }
         | RelationExpr "+" AddExpr { AddExpr Addition $1 $3 }
         | RelationExpr "-" AddExpr { AddExpr Substraction $1 $3 }
         | RelationExpr { AddRawExpr $1 }

RelationExpr : "(" RelationExpr ")" { $2 }
         | EqExpr ">" RelationExpr { RelationExpr Greater $1 $3 }
         | EqExpr "<" RelationExpr { RelationExpr Less $1 $3 }

         | EqExpr ">=" RelationExpr { RelationExpr GreaterOrEq $1 $3 }
         | EqExpr "<=" RelationExpr { RelationExpr LessOrEq $1 $3 }

         | EqExpr { RelationRawExpr $1 }

EqExpr : "(" EqExpr ")" { $2 }
         | AndExpr "==" EqExpr { EqExpr Equality $1 $3 }
         | AndExpr "!=" EqExpr { EqExpr Inequality $1 $3 }
         | AndExpr { EqRawExpr $1 }

AndExpr : "(" AndExpr ")" { $2 }
         | OrExpr "&&" AndExpr { AndExpr $1 $3 }
         | OrExpr { AndRawExpr $1 }

OrExpr : "(" OrExpr ")" { $2 }
         | BaseExpr "||" OrExpr { OrExpr $1 $3 }
         | BaseExpr { OrRawExpr $1 }

BaseExpr : "(" BaseExpr ")" { $2 }
         | FuncCall { $1 }
         | Value { ValueExpr $1 }
         | Id { VarExpr $1 }

Value : intVal { IntValue $1 }
      | floatVal { FloatValue $1 }
      | strVal { StringValue $1 }
      | boolVal { BoolValue $1 }

FuncCall : Id "(" FParams ")" { FunctionCall $1 $3 }

FParams : {- empty -} { [] }
      | Expr { [$1] }
      | Expr "," FParams { $1 : $3 }
     
LDecs : {- empty -} { [] }
      | LDec LDecs { $1 : $2 }

LDec : VarDec ";" { LocalVariableDeclaration $1 }
     | LoopDec { LoopDeclaration $1 }
     | IfDec { IfDeclaration $1 }
     | Expr ";" { LocalExpr $1 }
     | Return ";" { ReturnCall $1 }

LoopDec : for "(" ForVar ";" Cond ";" ForUpd ")" "{" LDecs "}"
          { ForLoop (For (ForHeader $3 $5 $7) $10) }
        | while "(" Cond ")" "{" LDecs "}" { WhileLoop (While $3 $6) }

ForVar : {- empty -} { Nothing }
       | VarDec { Just $1 }

Cond : {- empty -} { Nothing }
     | Expr { Just $1 }

ForUpd : {- empty -} { Nothing }
       | Id "=" Expr { Just (VariableUpdate $1 $3) }

IfDec : if "(" Cond ")" "{" LDecs "}" else "{" LDecs "}" { If $3 $6 $10 }
      | if "(" Cond ")" "{" LDecs "}" { If $3 $6 [] }

Return : return Expr { Return $2 }

Id : name { Identifier $1 }

{

parseError :: [Token] -> Either String a
parseError ts = Left
  $ mconcat [
    "Syntax error detected!\n\n"
  , "Line: "
  , show l
  , "\n"
  , "Column: "
  , show c
  , "\n"
  , "Token: "
  , toSourceCode tc
            ]
  where
    (Token (l, c) tc) = head ts

}
