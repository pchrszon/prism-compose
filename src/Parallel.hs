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

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements the parallel composition of Prism modules.
module Parallel
  ( Parallel(..)
  ) where

import Control.Applicative

import Data.Maybe
import Data.Monoid

import Syntax

-- | Monoid under parallel composition.
newtype Parallel = Parallel { getParallel :: Module () }

instance Monoid Parallel where
    mempty = Parallel emptyModule
    mappend (Parallel m1) (Parallel m2) = Parallel (compose m1 m2)

emptyModule :: Module ()
emptyModule = Module "" [] [] ()

compose :: Module () -> Module () -> Module ()
compose (Module name1 decls1 stmts1 _) (Module name2 decls2 stmts2 _) =
    let name  = name1 <> name2
        decls = decls1 <> decls2
        stmts = concatMap f stmts1 <> filter interleaved stmts2 -- keep ordering of first module
    in Module name decls stmts ()
  where
    f stmt
      | isNothing (stmtAction stmt) = [stmt]
      | otherwise = case filter ((stmtAction stmt ==) . stmtAction) stmts2 of
        []    -> [stmt]
        stmts -> fmap (composeStmts stmt) stmts
    interleaved stmt =
        let action = stmtAction stmt
        in isNothing (stmtAction stmt) ||
           not (any ((action ==) . stmtAction) stmts1)

composeStmts :: Stmt () -> Stmt () -> Stmt ()
composeStmts (Stmt action grd1 upds1 _) (Stmt _ grd2 upds2 _) =
    let grd  = grd1 `land` grd2
        upds = [ Update (prob1 `mulProb` prob2) (asgns1 <> asgns2) ()
               | Update prob1 asgns1 _ <- normalizeUpdates upds1
               , Update prob2 asgns2 _ <- normalizeUpdates upds2
               ]
    in Stmt action grd upds ()

normalizeUpdates :: [Update ()] -> [Update ()]
normalizeUpdates = \case
    []   -> [Update Nothing [] ()]
    upds -> upds

mulProb :: Probability () -> Probability () -> Probability ()
mulProb (Just l) (Just r) = Just $ l * r
mulProb l        r        = l <|> r

