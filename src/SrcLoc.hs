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
