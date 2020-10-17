{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Syntax.ASTSpec where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import qualified Data.Map                  as Map
import           Language.Lexer
import           Language.Parser
import           Language.Syntax.AST
import           Language.Syntax.Internals
import           Test.Hspec


spec :: Spec
spec = do
  describe "Interpreter specs" $ do

    it "Hello World" $ do
      f <- readFile "code_samples/hello-world.c"

      let ansState = TestState [] ["\"Hello world\""]
      let inState = TestState [] []

      let initState =
            AppState Map.empty Map.empty Map.empty Nothing Global
      let (Right ast) = parseTokens (scanTokens f)

      let result = runExceptT $ evalStateT (runApp $ interpret ast) initState

      execState (unwrapTestM result) inState `shouldBe` ansState


    it "Echo" $ do
      f <- readFile "code_samples/echo.c"

      let ansState = TestState [] ["Hello"]
      let inState = TestState ["Hello"] []

      let initState =
            AppState Map.empty Map.empty Map.empty Nothing Global
      let (Right ast) = parseTokens (scanTokens f)

      let result = runExceptT $ evalStateT (runApp $ interpret ast) initState

      execState (unwrapTestM result) inState `shouldBe` ansState


  describe "Pretty-Printer specs" $ do

    it "Hello World" $ do
      f <- readFile "code_samples/hello-world.c"

      let (Right ast) = parseTokens (scanTokens f)

      toSourceCode ast `shouldBe` "int main() {\n   printf(\"Hello world\");\n   return 0;\n}\n\n"


    it "Echo" $ do
      f <- readFile "code_samples/echo.c"

      let (Right ast) = parseTokens (scanTokens f)

      toSourceCode ast `shouldBe` "int main() {\n   string str = scanf();\n   printf(str);\n   return 0;\n}\n\n"



data TestState = TestState
  { inputs  :: [String]
  , outputs :: [String]
  } deriving (Show, Eq)

newtype TestM a = TestM {
  unwrapTestM :: State TestState a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState TestState
             )

instance MonadSTD TestM where
  readSTD = do
    s <- gets inputs
    modify (\(TestState (_:xs) y) -> TestState xs y)
    pure $ head s

  writeSTD s = modify (\(TestState xs ys) -> TestState xs (ys <> [s]))
