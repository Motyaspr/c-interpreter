module Main where

import           Control.Applicative       (Alternative ((<|>)), (<**>))
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.State.Lazy  (evalStateT)
import qualified Data.Map.Strict           as Map
import           Language.Lexer            (scanTokens)
import           Language.Parser           (parseTokens)
import           Language.Syntax.AST       (AppM (runApp), AppState (AppState),
                                            Interpretable (interpret),
                                            Scope (Global),
                                            showInterpreterError)
import           Language.Syntax.Internals (ToSourceCode (toSourceCode))
import           Options.Applicative       (Parser, execParser, flag, fullDesc,
                                            helper, info, long, metavar,
                                            progDesc, short, strArgument,
                                            strOption)

file :: String
file = "code_samples/hello-world.c"

data Mode = Interpreter
    | PrettyPrinter String

data AppConfig = AppConfig
    { sourceFile :: String
    , mode       :: Mode
    }

parseConfig :: Parser AppConfig
parseConfig = AppConfig
  <$> strArgument (metavar "SOURCE_FILE_NAME")
  <*> parseMode

parseMode :: Parser Mode
parseMode = flag Interpreter Interpreter (long "interpret" <> short 'i')
        <|> PrettyPrinter <$> strOption (  long "pretty-print"
                                        <> short 'p'
                                        <> metavar "TARGET_FILE_NAME"
                                        )

main :: IO ()
main = do
  execApp =<< execParser opts
  where
    opts = info (parseConfig <**> helper)
      ( fullDesc
     <> progDesc "Interpreter and pretty printer for C-like language"
      )


execApp :: AppConfig -> IO ()
execApp config = do
  sourceCode <- readFile $ sourceFile config

  let tokens = scanTokens sourceCode

  case (parseTokens tokens, mode config) of
    (Left s, _) ->
      putStrLn s

    (Right ast, Interpreter) -> do
      let initState =
            AppState Map.empty Map.empty Map.empty Nothing Global
      result <- runExceptT $ evalStateT (runApp $ interpret ast) initState
      either printErr (pure $ pure ()) result

    (Right ast, PrettyPrinter target) ->
      writeFile target (toSourceCode ast)

    where
      printErr e = putStrLn $ "\nERROR OCCURED: " <> showInterpreterError e
