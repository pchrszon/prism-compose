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

{-# LANGUAGE OverloadedStrings #-}

-- | The SrcLoc module defines the 'SrcLoc' type which is used to annotate
-- AST nodes with their location in the source file.

module SrcLoc
  ( SrcLoc(..)
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

-- | The position in a source file.
data SrcLoc = SrcLoc
  { locFile   :: !Text
  , locLine   :: !Int
  , locColumn :: !Int
  } deriving (Show)

instance Pretty SrcLoc where
    pretty (SrcLoc file y x) =
        dquotes (text file) <+> parens (
            "line:" <+> int y <> comma <+> "column:" <+> int x)
