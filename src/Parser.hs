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

{-# LANGUAGE FlexibleContexts #-}

-- | Provides a parser for Prism models.
module Parser
  ( parseModel
  ) where

import Data.Text.Lazy ( Text )

import Text.Parsec ( SourceName )

import Error
import Syntax

import Parser.Internal

parseModel :: MonadError Error m => SourceName -> Text -> m LModel
parseModel = parseFile model

