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

