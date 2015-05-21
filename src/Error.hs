{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Defines errors that can occur during translation.

module Error
  ( Error(..)
  , ErrorDesc(..)
  , throw

  , module Control.Monad.Except

  , _SyntaxError
  ) where

import Control.Lens.TH

import Control.Monad.Except

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import SrcLoc

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
  deriving (Show)

makePrisms ''ErrorDesc

instance Pretty ErrorDesc where
    pretty = \case
        SyntaxError msg ->
            string msg

