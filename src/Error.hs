{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines errors that can occur during translation.

module Error
  ( Error(..)
  , ErrorDesc(..)
  , throw

  , module Control.Monad.Except
  ) where

import Control.Monad.Except

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import SrcLoc
import Syntax

-- | Represents translation errors. It provides an error description and the
-- source location.
data Error = Error !SrcLoc !ErrorDesc deriving (Show)

instance Pretty Error where
    pretty (Error l desc) = pretty l <> colon <> line <> pretty desc

-- | Throws an 'Error'.
throw :: MonadError Error m => SrcLoc -> ErrorDesc -> m a
throw l = throwError . Error l

-- | Represents error messages that can occur during translation.
data ErrorDesc
  = SyntaxError !Text
  | UndefinedIdentifier !Name
  | CyclicDependency !Name !LExpr
  | IncompleteRenaming [Name]
  deriving (Show)

instance Pretty ErrorDesc where
    pretty = \case
        SyntaxError msg ->
            string msg
        UndefinedIdentifier name -> "undefined identifier" <+> text name
        CyclicDependency name e ->
            "cyclic dependency in the definition of" <+> text name <+>
            parens ("defined as" <+> pretty e)
        IncompleteRenaming names ->
            "the definition must rename the following variables" <> colon <>
            line <> sep (punctuate comma (map text names))

