{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module defines the operators that can be used in Prism expressions.

module Syntax.Operators
  ( BinOp(..)
  , ArithBinOp(..)
  , _ArithBinOp
  , EqBinOp(..)
  , _EqBinOp
  , RelBinOp(..)
  , _RelBinOp
  , LogicBinOp(..)
  , _LogicBinOp
  , binOpPrec
  , UnOp(..)
  , ArithUnOp(..)
  , _ArithUnOp
  , LogicUnOp(..)
  , _LogicUnOp
  , unOpPrec
  ) where

import Control.Lens.TH

import Text.PrettyPrint.Leijen.Text

data BinOp
  = ArithBinOp !ArithBinOp
  | EqBinOp !EqBinOp
  | RelBinOp !RelBinOp
  | LogicBinOp !LogicBinOp
  deriving (Eq, Show)

data ArithBinOp
  = Mul
  | Div
  | Add
  | Sub
  deriving (Bounded, Enum, Eq, Show)

data EqBinOp
  = Eq
  | Neq
  deriving (Bounded, Enum, Eq, Show)

data RelBinOp
  = Gt
  | Lt
  | Gte
  | Lte
  deriving (Bounded, Enum, Eq, Show)

data LogicBinOp
  = LAnd
  | LOr
  deriving (Bounded, Enum, Eq, Show)

makePrisms ''BinOp

-- | Returns the precedence level of the given operator.
binOpPrec :: BinOp -> Int
binOpPrec = \case
    ArithBinOp binOp -> case binOp of
        Mul     -> 10
        Div     -> 10
        Add     -> 9
        Sub     -> 9
    EqBinOp _   -> 8
    RelBinOp _  -> 8
    LogicBinOp binOp -> case binOp of
        LAnd    -> 5
        LOr     -> 4

data UnOp
  = ArithUnOp !ArithUnOp
  | LogicUnOp !LogicUnOp
  deriving (Eq, Show)

data ArithUnOp
  = Neg
  deriving (Bounded, Enum, Eq, Show)

data LogicUnOp
 = LNot
  deriving (Bounded, Enum, Eq, Show)

makePrisms ''UnOp

-- | Returns the precedence level of the given operator.
unOpPrec :: UnOp -> Int
unOpPrec = \case
    ArithUnOp _  -> 12
    LogicUnOp _  -> 6

instance Pretty BinOp where
    pretty = \case
        ArithBinOp binOp -> pretty binOp
        EqBinOp    binOp -> pretty binOp
        RelBinOp   binOp -> pretty binOp
        LogicBinOp binOp -> pretty binOp

instance Pretty ArithBinOp where
    pretty = \case
        Div -> "/"
        Mul -> "*"
        Sub -> "-"
        Add -> "+"

instance Pretty EqBinOp where
    pretty = \case
        Eq  -> "="
        Neq -> "!="

instance Pretty RelBinOp where
    pretty = \case
        Gt  -> ">"
        Lt  -> "<"
        Gte -> ">="
        Lte -> "<="

instance Pretty LogicBinOp where
    pretty = \case
        LAnd -> "&"
        LOr  -> "|"

instance Pretty UnOp where
    pretty unOpT = case unOpT of
        ArithUnOp unOp     -> pretty unOp
        LogicUnOp unOp     -> pretty unOp

instance Pretty ArithUnOp where
    pretty = \case
        Neg -> "-"

instance Pretty LogicUnOp where
    pretty = \case
        LNot -> "!"

