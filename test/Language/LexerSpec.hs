module Language.LexerSpec where

import           Language.Lexer
import           Language.Syntax.Token
import           Test.Hspec

spec :: Spec
spec = do
  describe "Lexer specs" $ do

    it "Hello World" $ do
      f <- readFile "code_samples/hello-world.c"

      let ans = [ TokenTypeDeclaration TokenTypeInt
                , TokenIndentifier "main"
                , TokenOpenRoundBracket
                , TokenCloseRoundBracket
                , TokenOpenCurlyBracket
                , TokenIndentifier "printf"
                , TokenOpenRoundBracket
                , TokenString "\"Hello world\""
                , TokenCloseRoundBracket
                , TokenSemicolon
                , TokenReturn
                , TokenInt 0
                , TokenSemicolon
                , TokenCloseCurlyBracket
                ]

      (\(Token _ x) -> x) <$> scanTokens f `shouldBe` ans

    it "Echo" $ do
      f <- readFile "code_samples/echo.c"

      let ans = [ TokenTypeDeclaration TokenTypeInt
                , TokenIndentifier "main"
                , TokenOpenRoundBracket
                , TokenCloseRoundBracket
                , TokenOpenCurlyBracket
                , TokenTypeDeclaration TokenTypeString
                , TokenIndentifier "str"
                , TokenAssignment
                , TokenIndentifier "scanf"
                , TokenOpenRoundBracket
                , TokenCloseRoundBracket
                , TokenSemicolon
                , TokenIndentifier "printf"
                , TokenOpenRoundBracket
                , TokenIndentifier "str"
                , TokenCloseRoundBracket
                , TokenSemicolon
                , TokenReturn
                , TokenInt 0
                , TokenSemicolon
                , TokenCloseCurlyBracket
                ]

      (\(Token _ x) -> x) <$> scanTokens f `shouldBe` ans
