{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

-- | The preprocessor handles the substitution of formulas and the renaming
-- of modules.
module Preprocessor
  ( preprocess
  ) where

import Control.Arrow ( (&&&) )
import Control.Lens
import Control.Monad

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( difference )
import qualified Data.Set as Set
import Data.Traversable

import Error
import SrcLoc
import Syntax

-- | Performs formula substitution and module renaming. The resulting list
-- will not contain any 'RenameDef's.
preprocess :: MonadError Error m => [LDefinition] -> m [LDefinition]
preprocess defs = do
    let frmDefs = formulaDefs defs
    checkIfNonCyclic frmDefs
    resolveRenamings (fmap (substituteFormulas frmDefs) defs)

substituteFormulas :: HasExprs t => Map Name LExpr -> t a -> t a
substituteFormulas frmDefs = over exprs (rewrite substitute)
  where
    substitute e = case e of
        IdentExpr name l -> frmDefs^?at name._Just.to (l <$) -- annotate the substituted expression with the annotation (location) of the replaced formula identifier
        _                -> Nothing

resolveRenamings :: MonadError Error m => [LDefinition] -> m [LDefinition]
resolveRenamings defs =
    let mods = Map.fromList .
               map (\m -> (modName m, m)) $ defs^..traverse._ModuleDef
    in for defs $ \def -> case def of
        RenameDef r -> ModuleDef <$> renameModule mods r
        _           -> return def

renameModule :: MonadError Error m => Map Name LModule -> LRenaming -> m LModule
renameModule mods rn = case mods^.at (rnSource rn) of
    Just m  -> do
        checkRenamings rn m
        return $ applyRenamings rn m
    Nothing -> throw (rnAnnot rn) $ UndefinedIdentifier (rnSource rn)

checkRenamings :: MonadError Error m => Renaming SrcLoc -> Module a -> m ()
checkRenamings rn m =
    let vars        = Set.fromList . map declName $ modVars m
        renamedVars = Set.fromList . map fst $ rnRenamings rn
        missingVars = vars `difference` renamedVars
    in unless (Set.null missingVars) . throw (rnAnnot rn) $
        IncompleteRenaming (Set.toList missingVars)

applyRenamings :: Renaming a -> Module a -> Module a
applyRenamings (Renaming name _ rns a) m =
    let m' = a <$ over idents (replaceWith (Map.fromList rns)) m -- replace identifiers and overwrite annotations
    in m' { modName = name }
  where
    replaceWith rns' ident = Map.findWithDefault ident ident rns' -- if ident is in the rns' Map, rename it

formulaDefs :: [LDefinition] -> Map Name LExpr
formulaDefs =
    Map.fromList . fmap (frmName &&& frmExpr) . toListOf (traverse._FormulaDef)

-- | @checkIfNonCyclic defs@ checks if any of the definitions in @defs@ is
-- defined in terms of itself. In case a cyclic dependency is found,
-- a 'CyclicDependency' error is thrown.
checkIfNonCyclic :: MonadError Error m => Map Name LExpr -> m ()
checkIfNonCyclic defs =
    void . for (Map.assocs defs) $ \(n, e) ->
        when (isCyclicDef defs n) . throw (exprAnnot e) $ CyclicDependency n e

-- | Given a 'Map' of definitions (@name = expr@) and the name of
-- a definition, @isCyclicDef@ returns 'True' if the definition is cyclic,
-- i.e. it is defined in terms of itself. This function returns 'False' if
-- it is given a name not contained in @defs@.
isCyclicDef :: Map Name (Expr a) -> Name -> Bool
isCyclicDef defs = isCyclic Set.empty
  where
    isCyclic ns n                -- ns: defined names visited so far, n: name of current definition
      | n `Set.member` ns = True -- definition has already been visited -> cycle detected
      | otherwise =
          case defs^.at n of
            Just e ->
                let ns' = Set.insert n ns
                in anyOf (traverse._IdentExpr._1) (isCyclic ns') $ universe e
            Nothing -> False

