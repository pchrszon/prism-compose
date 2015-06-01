{-
compose: parallel composition of Prism modules
Copyright (C) 2015 Philipp Chrszon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
    pretty (Error l desc) = pretty l <+> pretty desc

-- | Throws an 'Error'.
throw :: MonadError Error m => SrcLoc -> ErrorDesc -> m a
throw l = throwError . Error l

-- | Represents error messages that can occur during translation.
data ErrorDesc
  = SyntaxError !Text
  | UndefinedIdentifier !Name
  | UndefinedModule !Name
  | CyclicDependency !Name !LExpr
  | IncompleteRenaming [Name]
  deriving (Show)

instance Pretty ErrorDesc where
    pretty = \case
        SyntaxError msg -> string msg
        UndefinedIdentifier name -> "undefined identifier" <+> text name
        UndefinedModule name -> "module" <+> text name <+> "does not exist"
        CyclicDependency name e ->
            "cyclic dependency in the definition of" <+> text name <+>
            parens ("defined as" <+> pretty e)
        IncompleteRenaming names ->
            "the definition must rename the following variables" <> colon <>
            line <> sep (punctuate comma (map text names))

