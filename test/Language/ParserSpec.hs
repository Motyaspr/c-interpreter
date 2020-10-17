module Language.ParserSpec where

import           Language.Lexer
import           Language.Parser
import           Language.Syntax.AST

import           Test.Hspec

spec :: Spec
spec = do
  describe "Lexer specs" $ do

    it "Hello World" $ do
      f <- readFile "code_samples/hello-world.c"

      let ans = Right $ AST [FunctionDeclaration (Function {funcName = Identifier "main", funcType = TypeInt, funcArgs = [], funcBody = [LocalExpr (Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (FunctionCall (Identifier "printf") [Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (ValueExpr (StringValue "\"Hello world\"")))))))))]))))))))),ReturnCall (Return (Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (ValueExpr (IntValue 0)))))))))))]})]

      parseTokens (scanTokens f) `shouldBe` ans

    it "Echo" $ do
      f <- readFile "code_samples/echo.c"

      let ans = Right $ AST [FunctionDeclaration (Function {funcName = Identifier "main", funcType = TypeInt, funcArgs = [], funcBody = [LocalVariableDeclaration (VariableDeclaration (Variable {varName = Identifier "str", varType = TypeString, varValue = Nothing}) (Just (Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (FunctionCall (Identifier "scanf") []))))))))))),LocalExpr (Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (FunctionCall (Identifier "printf") [Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (VarExpr (Identifier "str")))))))))]))))))))),ReturnCall (Return (Expr (UnaryRawExpr (MultRawExpr (AddRawExpr (RelationRawExpr (EqRawExpr (AndRawExpr (OrRawExpr (ValueExpr (IntValue 0)))))))))))]})]

      parseTokens (scanTokens f) `shouldBe` ans
