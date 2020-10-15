module Language.Syntax.Internals where

class ToSourceCode a where
  toSourceCode :: a -> String

class Castable a where
  castToInt :: a -> a
  castToFloat :: a -> a
  castToString :: a -> a
  castToBool :: a -> a
