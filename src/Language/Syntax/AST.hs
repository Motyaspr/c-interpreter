{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Language.Syntax.AST where

import           Control.Applicative       (Alternative ((<|>)))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            MonadIO (..))
import           Control.Monad.State.Lazy  (MonadState (get), StateT (StateT),
                                            modify)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Language.Syntax.Internals (Castable (..), ToSourceCode (..))
import           Text.Read                 (readMaybe)

-- Abstract syntax tree a.k.a eDSL
newtype AST = AST [GlobalDeclaration] deriving (Eq, Show)

data GlobalDeclaration = FunctionDeclaration Function
    | GlobalVariableDeclaration VariableDeclaration
    | GlobalExpr Expr
    deriving (Eq, Show)

data Variable = Variable
    { varName  :: Identifier
    , varType  :: Type
    , varValue :: Maybe Value
    }
    deriving (Eq, Show)

newtype Expr = Expr UnaryExpr deriving (Eq, Show)

data UnaryExpr = UnaryExpr UnaryAction UnaryExpr
    | UnaryRawExpr MultExpr
    deriving (Eq, Show)

data UnaryAction = UnaryPlus
    | UnaryMinus
    | UnaryNot
    deriving (Eq, Show)

data MultExpr = MultExpr MultAction AddExpr MultExpr
    | MultRawExpr AddExpr
    deriving (Eq, Show)

data MultAction = Multiply
    | Divide
    deriving (Eq, Show)

data AddExpr = AddExpr AddAction RelationExpr AddExpr
    | AddRawExpr RelationExpr
    deriving (Eq, Show)

data AddAction = Addition
    | Substraction
    deriving (Eq, Show)

data RelationExpr = RelationExpr RelationAction EqExpr RelationExpr
    | RelationRawExpr EqExpr
    deriving (Eq, Show)

data RelationAction = Greater
    | Less
    | GreaterOrEq
    | LessOrEq
    deriving (Eq, Show)

data EqExpr = EqExpr EqAction AndExpr EqExpr
    | EqRawExpr AndExpr
    deriving (Eq, Show)

data EqAction = Equality
    | Inequality
    deriving (Eq, Show)

data AndExpr = AndExpr OrExpr AndExpr
    | AndRawExpr OrExpr
    deriving (Eq, Show)

data OrExpr = OrExpr BaseExpr OrExpr
    | OrRawExpr BaseExpr
    deriving (Eq, Show)

data BaseExpr = ValueExpr Value
    | VarExpr Identifier
    | FunctionCall Identifier [Expr]
    deriving (Eq, Show)

data Type = TypeInt
    | TypeFloat
    | TypeString
    | TypeBool
    deriving (Eq, Show)

data Value = IntValue Int
    | FloatValue Float
    | StringValue String
    | BoolValue Bool
    deriving (Eq, Show)

newtype Identifier = Identifier String deriving (Eq, Show, Ord)

data Function = Function
    { funcName :: Identifier
    , funcType :: Type
    , funcArgs :: [Variable]
    , funcBody :: [LocalDeclaration]
    }
    deriving (Eq, Show)

data LocalDeclaration = LocalVariableDeclaration VariableDeclaration
    | LoopDeclaration Loop
    | IfDeclaration If
    | LocalExpr Expr
    | ReturnCall Return
    deriving (Eq, Show)

newtype Return = Return Expr deriving (Eq, Show)

data If = If
    { ifCond   :: Maybe Expr
    , ifBody   :: [LocalDeclaration]
    , elseBody :: [LocalDeclaration]
    }
    deriving (Eq, Show)

data VariableDeclaration = VariableDeclaration Variable (Maybe Expr)
    | VariableAssignment VariableUpdate
    deriving (Eq, Show)

data VariableUpdate = VariableUpdate Identifier Expr
    deriving (Eq, Show)

data Loop = ForLoop For
    | WhileLoop While
    deriving (Eq, Show)

data For = For
    { forHeader :: ForHeader
    , forBody   :: [LocalDeclaration]
    }
    deriving (Eq, Show)

data ForHeader = ForHeader
    { forVar  :: Maybe VariableDeclaration
    , forCond :: Maybe Expr
    , forUpd  :: Maybe VariableUpdate
    }
    deriving (Eq, Show)

data While = While
    { whileHeader :: Maybe Expr
    , whileBody   :: [LocalDeclaration]
    }
    deriving (Eq, Show)





-- toSourceCode transforms ast to pretty-printed string
instance ToSourceCode AST where
  toSourceCode (AST gds) = foldMap (\gd -> toSourceCode gd <> "\n\n") gds

instance ToSourceCode GlobalDeclaration where
  toSourceCode (FunctionDeclaration f)       = toSourceCode f
  toSourceCode (GlobalVariableDeclaration v) = toSourceCode v <> ";"
  toSourceCode (GlobalExpr e)                = toSourceCode e <> ";"

instance ToSourceCode Function where
  toSourceCode (Function id' type' args body) = mconcat [
      toSourceCode type'
    , " "
    , toSourceCode id'
    , "("
    , printArgs args
    , ") {\n"
    , printLocalDecs body
    , "}"
                                                        ]
    where
      printArgs []     = ""
      printArgs [v]    = toSourceCode v
      printArgs (v:vs) = toSourceCode v <> ", " <> printArgs vs

instance ToSourceCode LocalDeclaration where
  toSourceCode (LocalVariableDeclaration v) = toSourceCode v <> ";"
  toSourceCode (LoopDeclaration l)          = toSourceCode l
  toSourceCode (IfDeclaration e)            = toSourceCode e
  toSourceCode (LocalExpr e)                = toSourceCode e <> ";"
  toSourceCode (ReturnCall r)               = toSourceCode r <> ";"

instance ToSourceCode Return where
  toSourceCode (Return e) = "return " <> toSourceCode e

instance ToSourceCode VariableDeclaration where
  toSourceCode (VariableDeclaration v Nothing) = toSourceCode v
  toSourceCode (VariableDeclaration v (Just e)) = toSourceCode v <> " = " <> toSourceCode e
  toSourceCode (VariableAssignment v) = toSourceCode v

instance ToSourceCode VariableUpdate where
  toSourceCode (VariableUpdate id' e) = toSourceCode id' <> " = " <> toSourceCode e

instance ToSourceCode Variable where
  toSourceCode (Variable id' t' _) = toSourceCode t' <> " " <> toSourceCode id'

instance ToSourceCode Type where
  toSourceCode TypeInt    = "int"
  toSourceCode TypeFloat  = "float"
  toSourceCode TypeString = "string"
  toSourceCode TypeBool   = "bool"

instance ToSourceCode Loop where
  toSourceCode (ForLoop l)   = toSourceCode l
  toSourceCode (WhileLoop l) = toSourceCode l

instance ToSourceCode For where
  toSourceCode (For h b) = toSourceCode h
    <> " {\n" <> printLocalDecs b <> "}"

instance ToSourceCode ForHeader where
  toSourceCode (ForHeader var cond upd) = "for("
    <> mbToStr var <> "; " <> mbToStr cond <> mbToStr upd <> ")"

instance ToSourceCode While where
  toSourceCode (While h b) = "while(" <> mbToStr h <> ") {\n" <> printLocalDecs b <> "}"

instance ToSourceCode If where
  toSourceCode (If h b eb) = "if(" <> mbToStr h <> ") {\n"
    <> printLocalDecs b <> "}" <> printElse eb
    where
      printElse [] = ""
      printElse ds = "\n else {" <> printLocalDecs ds <> "}"

instance ToSourceCode Expr where
  toSourceCode (Expr e) = toSourceCode e

instance ToSourceCode UnaryExpr where
  toSourceCode (UnaryExpr UnaryPlus e)  = "+(" <> toSourceCode e <> ")"
  toSourceCode (UnaryExpr UnaryMinus e) = "-(" <> toSourceCode e <> ")"
  toSourceCode (UnaryExpr UnaryNot e)   = "!(" <> toSourceCode e <> ")"
  toSourceCode (UnaryRawExpr e)         = toSourceCode e

instance ToSourceCode MultExpr where
  toSourceCode (MultExpr Multiply l r) = toSourceCode l <> " * " <> toSourceCode r
  toSourceCode (MultExpr Divide l r) = toSourceCode l <> " / " <> toSourceCode r
  toSourceCode (MultRawExpr e) = toSourceCode e

instance ToSourceCode AddExpr where
  toSourceCode (AddExpr Addition l r) = toSourceCode l <> " * " <> toSourceCode r
  toSourceCode (AddExpr Substraction l r) = toSourceCode l <> " / " <> toSourceCode r
  toSourceCode (AddRawExpr e) = toSourceCode e

instance ToSourceCode RelationExpr where
  toSourceCode (RelationExpr Greater l r) = toSourceCode l <> " > " <> toSourceCode r
  toSourceCode (RelationExpr Less l r) = toSourceCode l <> " < " <> toSourceCode r
  toSourceCode (RelationExpr GreaterOrEq l r) = toSourceCode l <> " >= " <> toSourceCode r
  toSourceCode (RelationExpr LessOrEq l r) = toSourceCode l <> " <= " <> toSourceCode r
  toSourceCode (RelationRawExpr e) = toSourceCode e

instance ToSourceCode EqExpr where
  toSourceCode (EqExpr Equality l r) = toSourceCode l <> " == " <> toSourceCode r
  toSourceCode (EqExpr Inequality l r) = toSourceCode l <> " != " <> toSourceCode r
  toSourceCode (EqRawExpr e) = toSourceCode e

instance ToSourceCode AndExpr where
  toSourceCode (AndExpr l r)  = toSourceCode l <> " && " <> toSourceCode r
  toSourceCode (AndRawExpr e) = toSourceCode e

instance ToSourceCode OrExpr where
  toSourceCode (OrExpr l r)  = toSourceCode l <> " || " <> toSourceCode r
  toSourceCode (OrRawExpr e) = toSourceCode e

instance ToSourceCode BaseExpr where
  toSourceCode (ValueExpr v) = toSourceCode v
  toSourceCode (VarExpr v) = toSourceCode v
  toSourceCode (FunctionCall id' params) = mconcat [
    toSourceCode id'
    , "("
    , printParams params
    , ")"
                                                   ]
    where
      printParams []     = []
      printParams [p]    = toSourceCode p
      printParams (p:ps) = toSourceCode p <> ", " <> printParams ps

instance ToSourceCode Value where
  toSourceCode (IntValue v)    = show v
  toSourceCode (FloatValue v)  = show v
  toSourceCode (StringValue v) = v
  toSourceCode (BoolValue v)   = show v

instance ToSourceCode Identifier where
  toSourceCode (Identifier s) = s

printLocalDecs :: [LocalDeclaration] -> String
printLocalDecs = foldMap (\d -> "   " <> toSourceCode d <> "\n")

mbToStr :: ToSourceCode a => Maybe a -> String
mbToStr Nothing  = ""
mbToStr (Just x) = toSourceCode x





-- Cast types
instance Castable Value where
  castToInt (IntValue x)   = IntValue x
  castToInt (FloatValue x) = IntValue $ round x
  castToInt (StringValue s) = IntValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = length s
      mbToInt (Just y) = y
  castToInt (BoolValue True) = IntValue 1
  castToInt (BoolValue False) = IntValue 0

  castToFloat (IntValue x)   = FloatValue $ fromIntegral x
  castToFloat (FloatValue x) = FloatValue x
  castToFloat (StringValue s) = FloatValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = fromIntegral $ length s
      mbToInt (Just y) = y
  castToFloat (BoolValue True)  = FloatValue 1
  castToFloat (BoolValue False) = FloatValue 0

  castToString (IntValue x)      = StringValue $ show x
  castToString (FloatValue x)    = StringValue $ show x
  castToString (StringValue s)   = StringValue s
  castToString (BoolValue True)  = StringValue "1"
  castToString (BoolValue False) = StringValue ""

  castToBool (IntValue x)
    | x > 0 = BoolValue True
    | otherwise = BoolValue False
  castToBool (FloatValue x)
    | x > 0 = BoolValue True
    | otherwise = BoolValue False
  castToBool (StringValue s)
    | s == "" = BoolValue False
    | otherwise = BoolValue True
  castToBool (BoolValue x) = BoolValue x






-- Interpreting ast
data Scope = Local
    | Global
    deriving (Eq, Show)

data AppState = AppState
    { funcs       :: Map Identifier Function
    , globalVars  :: Map Identifier Variable
    , localVars   :: Map Identifier Variable
    , currentFunc :: Maybe Function
    , scope       :: Scope
    }

newtype AppM a = AppM {
  runApp :: StateT AppState (ExceptT String IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError String
             , MonadState AppState
             , MonadIO
             )


class Interpretable a b | a -> b where
  interpret :: a -> AppM b


instance Interpretable AST () where
  interpret (AST ds) = do
    mapM_ interpret ds
    fs <- funcs <$> get
    case Map.lookup (Identifier "main") fs of
      Just f ->
        interpret f >> pure ()
      Nothing ->
        throwError "\nERROR OCCURRED: main function is not defined."


instance Interpretable GlobalDeclaration () where
  interpret (FunctionDeclaration f) = do
    let id' = funcName f
    fs <- funcs <$> get
    case (Map.member id' fs, Map.member id' buildInFunctions) of
      (False, False) -> do
        let fs' =  Map.insert id' f fs
        modify (\x -> x { funcs = fs' })
      (True, _) ->
        throwError
        $ "Function with name " <> toSourceCode id' <> " was already defined."
      (False, _) ->
        throwError
        $ "Function name " <> toSourceCode id' <> " is reserved."

  interpret (GlobalVariableDeclaration v) =
    modify (\x -> x { scope = Global }) >> interpret v

  interpret (GlobalExpr e) = interpret e >> pure ()


instance Interpretable Expr Value where
  interpret (Expr e) = interpret e


instance Interpretable UnaryExpr Value where
  interpret (UnaryExpr UnaryPlus e) = interpret e
  interpret (UnaryExpr UnaryMinus e) =
    interpret e >>= (handle . castToFloat)
    where
      handle (FloatValue x) = pure $ FloatValue $ (-1) * x
      handle x = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode x

  interpret (UnaryExpr UnaryNot e) =
    interpret e >>= (handle . castToFloat)
    where
      handle (BoolValue x) = pure $ BoolValue $ not x
      handle x = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode x

  interpret (UnaryRawExpr e) = interpret e


instance Interpretable MultExpr Value where
  interpret e@(MultExpr Multiply l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x * y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret e@(MultExpr Divide l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x / y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret (MultRawExpr e) = interpret e


instance Interpretable AddExpr Value where
  interpret e@(AddExpr Addition l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x + y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret e@(AddExpr Substraction l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ FloatValue $ x - y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret (AddRawExpr e) = interpret e


instance Interpretable RelationExpr Value where
  interpret e@(RelationExpr Greater l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x > y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr Less l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x < y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr GreaterOrEq l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x >= y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret e@(RelationExpr LessOrEq l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (FloatValue x) (FloatValue y) = pure $ BoolValue $ x <= y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret (RelationRawExpr e) = interpret e


instance Interpretable EqExpr Value where
  interpret (EqExpr Equality l r) = do
    l' <- interpret l
    r' <- interpret r
    pure $ handle l' r'
    where
      handle (FloatValue x) y  = BoolValue $ x == toFloat y
      handle (IntValue x) y    = BoolValue $ x == toInt y
      handle (StringValue x) y = BoolValue $ x == toString y
      handle (BoolValue x) y   = BoolValue $ x == toBool y
      toFloat y = (\(FloatValue x) -> x) $ castToFloat y
      toInt y = (\(IntValue x) -> x) $ castToInt y
      toString y = (\(StringValue x) -> x) $ castToString y
      toBool y = (\(BoolValue x) -> x) $ castToBool y

  interpret (EqExpr Inequality l r) = do
    l' <- interpret l
    r' <- interpret r
    pure $ handle l' r'
    where
      handle (FloatValue x) y  = BoolValue $ x /= toFloat y
      handle (IntValue x) y    = BoolValue $ x /= toInt y
      handle (StringValue x) y = BoolValue $ x /= toString y
      handle (BoolValue x) y   = BoolValue $ x /= toBool y
      toFloat y = (\(FloatValue x) -> x) $ castToFloat y
      toInt y = (\(IntValue x) -> x) $ castToInt y
      toString y = (\(StringValue x) -> x) $ castToString y
      toBool y = (\(BoolValue x) -> x) $ castToBool y

  interpret (EqRawExpr e) = interpret e



instance Interpretable AndExpr Value where
  interpret e@(AndExpr l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ BoolValue $ x && y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret (AndRawExpr e) = interpret e


instance Interpretable OrExpr Value where
  interpret e@(OrExpr l r) = do
    l' <- castToFloat <$> interpret l
    r' <- castToFloat <$> interpret r
    handle l' r'
    where
      handle (BoolValue x) (BoolValue y) = pure $ BoolValue $ x || y
      handle _ _ = throwError $ "\nERROR OCCURRED: Type casting error at: " <> toSourceCode e

  interpret (OrRawExpr e) = interpret e


data SpecialFunction = PrintF
    | ScanF
    deriving (Eq, Show)

buildInFunctions :: Map Identifier SpecialFunction
buildInFunctions = Map.fromList [
    (Identifier "printf", PrintF)
  , (Identifier "scanf", ScanF)
  ]


instance Interpretable BaseExpr Value where
  interpret (ValueExpr v) = interpret v

  interpret (VarExpr id') = do
    appState <- get
    let gvs = globalVars appState
    let lvs = localVars appState
    case (Map.lookup id' lvs) <|> (Map.lookup id' gvs) of
      Nothing ->
         throwError $ "\nERROR OCCURRED: Unknown variable: " <> toSourceCode id'
      Just v ->
         interpret v

  interpret (FunctionCall id' params) = do
    appState <- get
    let fs = funcs appState
    case (Map.lookup id' buildInFunctions, Map.lookup id' fs) of
      (Just PrintF, _) -> do
        str <- mconcat
          <$> mapM ((fmap ((\(StringValue s) -> s) . castToString)) . interpret)
          params
        liftIO $ putStrLn str
        pure $ IntValue $ length str

      (Just ScanF, _) ->
        liftIO $ StringValue <$> getLine

      (Nothing, Nothing) ->
        throwError $ "\nERROR OCCURRED: Unknown function: " <> toSourceCode id'

      (Nothing, Just f) ->
        if length (funcArgs f) /= length params then
          throwError
          $ "Wrong number of arguments for the following function: "
          <> toSourceCode f
        else do
          vars <- mapM (uncurry plugExprToVar)
                  $ zip params (funcArgs f)
          interpret (f {funcArgs = vars})

plugExprToVar :: Expr -> Variable -> AppM Variable
plugExprToVar expr var = do
  val <- interpret expr
  pure $ var {varValue = Just val}


instance Interpretable Function Value where
  interpret f@(Function id' _ args ds) = do
    baseF <- currentFunc <$> get
    modify (\x -> x { currentFunc = Just f, localVars = varsToMap args })
    r <- interpret ds
    modify (\x -> x { currentFunc = baseF, localVars = Map.empty })
    case r of
      Just v ->
        pure v
      Nothing ->
        throwError $ "\nERROR OCCURRED: Function " <> toSourceCode id' <> " has no return statement."
    where
      varsToMap :: [Variable] -> Map Identifier Variable
      varsToMap vars = Map.fromList $ zip (varName <$> vars) vars


instance Interpretable [LocalDeclaration] (Maybe Value) where
  interpret []                   = pure Nothing
  interpret (r@(ReturnCall _):_) = interpret r
  interpret (d:ds)               = interpret d >> interpret ds


instance Interpretable LocalDeclaration (Maybe Value) where
  interpret (ReturnCall r) = Just <$> interpret r
  interpret (LocalExpr e) = interpret e >> pure Nothing
  interpret (LocalVariableDeclaration v) = do
    modify (\x -> x { scope = Local })
    interpret v
    pure Nothing
  interpret (IfDeclaration e) = interpret e
  interpret (LoopDeclaration l) = interpret l


instance Interpretable Return Value where
  interpret (Return e) = interpret e


instance Interpretable VariableDeclaration () where
  interpret (VariableDeclaration var Nothing) = do
    s <- scope <$> get
    case s of
      Local -> do
        modify addLocal
      Global -> do
        modify addGlobal
    where
      addLocal t = t {
        localVars = Map.singleton (varName var) var <> localVars t
        }
      addGlobal t = t {
        globalVars = Map.singleton (varName var) var <> globalVars t
        }

  interpret (VariableDeclaration var (Just e)) = do
    s <- scope <$> get
    var' <- plugExprToVar e var
    case s of
      Local -> do
        modify (\t -> t {
          localVars = Map.singleton (varName var) var' <> localVars t
        })
      Global -> do
        modify (\t -> t {
          globalVars = Map.singleton (varName var) var' <> globalVars t
        })

  interpret (VariableAssignment v) = interpret v


instance Interpretable If (Maybe Value) where
  interpret (If (Just cond) ib eb) = do
    v' <- interpret cond
    let (BoolValue cond') = castToBool v'
    if cond' then interpret ib else interpret eb

  interpret (If Nothing ib _) = do
    interpret ib


instance Interpretable Loop (Maybe Value) where
  interpret (WhileLoop l) = interpret l
  interpret (ForLoop l)   = interpret l


instance Interpretable While (Maybe Value) where
  interpret l@(While Nothing b) =
    interpret b >> interpret l

  interpret l@(While (Just cond) b) = do
    v <- interpret cond
    b' <- interpret b
    case (castToBool v, b') of
      (BoolValue False, _) ->
        pure Nothing
      (BoolValue True, Nothing) ->
        interpret l
      (BoolValue True, r) ->
        pure r
      (_, _) ->
        throwError $ "\nERROR OCCURRED: Type cast error of " <> toSourceCode cond


instance Interpretable For (Maybe Value) where
  interpret (For (ForHeader v c vu) b) = evalV v >> loop
    where
      evalV (Just v') = interpret v' >> pure Nothing
      evalV Nothing   = pure Nothing
      evalC (Just c') = castToBool <$> interpret c'
      evalC Nothing   = pure $ BoolValue True
      loop = do
        cond <- evalC c
        b' <- interpret b
        case (cond, b') of
          (BoolValue False, _) ->
            pure Nothing
          (BoolValue True, Nothing) ->
            evalV vu >> loop
          (BoolValue True, r) ->
            pure r
          (_, _) ->
            throwError $ "\nERROR OCCURRED: Type cast error of " <> toSourceCode cond


instance Interpretable VariableUpdate () where
  interpret (VariableUpdate id' e) = do
    locals <- localVars <$> get
    globals <- globalVars <$> get

    case (Map.lookup id' locals, Map.lookup id' globals) of
      (Just v, _) -> do
        v' <- plugExprToVar e v
        let updatedLocals = Map.update (\_ -> Just v') id' locals
        modify (\x -> x { localVars = updatedLocals })
      (_, Just v) -> do
        v' <- plugExprToVar e v
        let updatedGlobals = Map.update (\_ -> Just v') id' globals
        modify (\x -> x { globalVars = updatedGlobals })
      (_, _) ->
        throwError $ "\nERROR OCCURRED: Variable "
        <> toSourceCode id' <> " is not defined."


instance Interpretable Variable Value where
  interpret (Variable _ TypeInt (Just v)) = castToInt <$> interpret v
  interpret (Variable _ TypeFloat (Just v)) = castToFloat <$> interpret v
  interpret (Variable _ TypeString (Just v)) = castToString <$> interpret v
  interpret (Variable _ TypeBool (Just v)) = castToBool <$> interpret v
  interpret (Variable id' _ Nothing) = throwError
    $ "\nERROR OCCURED: Variable " <> toSourceCode id' <> " has no value."

instance Interpretable Value Value where
  interpret v = pure v
