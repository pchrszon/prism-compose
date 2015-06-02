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

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Compose
  ( composeMain
  ) where

import Control.Exception.Lens
import Control.Lens hiding ( argument )

import Data.List ( (\\), partition )
import Data.Maybe
import Data.Text.Lazy ( Text, pack )
import qualified Data.Text.Lazy.IO as L
import Data.Version ( showVersion )

import Options.Applicative

import System.Exit
import System.IO
import System.IO.Error.Lens

import Text.PrettyPrint.Leijen.Text ( Pretty, pretty, renderPretty, displayT )

import Error
import Parallel
import Parser
import Preprocessor
import SrcLoc
import Syntax

import Paths_compose ( version )

appVersion :: String
appVersion = showVersion version

-- Compose CLI
--
-- compose [OPTIONS] MODEL [MODULES]
--
-- MODEL
helpModelFile = "The model file (if '-' is given, the model is read from stdin)"
--
-- options:
--    -o OUTPUT
helpModelOutput = "Write the model to file OUTPUT"
--    --merge-globals
helpMergeGlobals = "Move global variables into the module after composition"

data AppOptions = AppOptions
  { optInModelPath  :: FilePath
  , optOutModelPath :: Maybe FilePath
  , optMergeGlobals :: !Bool
  , optModules      :: [String]
  } deriving (Show)

appOptions :: Parser AppOptions
appOptions = AppOptions
    <$> strArgument         ( metavar "MODEL"
                           <> help helpModelFile )
    <*> optional (strOption ( short 'o'
                           <> metavar "OUTPUT"
                           <> help helpModelOutput ))
    <*> switch              ( long "merge-globals"
                           <> help helpMergeGlobals )
    <*> many (strArgument   ( metavar "MODULES..." ))

-- | The entry point of the application.
composeMain :: IO ()
composeMain = handling _IOException ioeHandler $
    execParser opts >>= composeWithOptions
  where
    opts = info (helper <*> versionOpt <*> appOptions) mempty
    versionOpt = infoOption appVersion ( long "version"
                                      <> hidden
                                      <> help "Display version information" )
    ioeHandler e = do
        let file = fromMaybe "<unknown source>" $ e^.fileName
        putStrLn $ e^.description ++ ": " ++ file
        exitWith $ ExitFailure 2

composeWithOptions :: AppOptions -> IO ()
composeWithOptions opts@AppOptions{..} = withHandles opts $ \(hIn, hOut) -> do
    let moduleNames = pack <$> optModules
    inModel <- L.hGetContents hIn

    let result = do
            model <- composeModules moduleNames <=<
                     preprocess <=<
                     parseModel optInModelPath $ inModel
            if optMergeGlobals then mergeGlobals model else return model

    case result of
        Left err -> do
            print $ pretty err
            exitWith $ ExitFailure 1
        Right outModel -> L.hPutStrLn hOut (render outModel)

composeModules :: MonadError Error m => [Name] -> LModel -> m (Model ())
composeModules moduleNames model@(Model modelT defs) = do
    let modelModules = defs^..traverse._ModuleDef.to modName

    if null modelModules
    then return $ void model
    else do
        case moduleNames \\ modelModules of
            m:_ -> throw NoLoc $ UndefinedModule m
            []  -> return ()

        let name:others = if null moduleNames then modelModules else moduleNames
            defs'       = void <$> defs
            otherMods   = filter ((`elem` others) . modName) $
                          defs'^..traverse._ModuleDef

        return . Model modelT . flip mapMaybe defs' $ \case
            ModuleDef m
              | modName m `elem` others -> Nothing
              | modName m == name ->
                Just . ModuleDef . getParallel . foldMap Parallel $ m:otherMods
              | otherwise -> Just $ ModuleDef m
            def -> Just def

mergeGlobals :: MonadError Error m => Model a -> m (Model a)
mergeGlobals (Model modelT defs) = do
    when (lengthOf (traverse._ModuleDef) defs > 1) $
        throw NoLoc IllegalGlobalsMerge

    let (defs', globalDefs) = partition (isn't _GlobalDef) defs
        globals             = globalDefs^..traverse._GlobalDef

    return . Model modelT $ over (traverse._ModuleDef) (merge globals) defs'
  where
    merge decls m = m { modVars = decls ++ modVars m }

withHandles :: AppOptions -> ((Handle, Handle) -> IO ()) -> IO ()
withHandles AppOptions{..} m =
    withSourceModel optInModelPath $ \im ->
    maybeWithFile WriteMode optOutModelPath $ \om ->
    m (im, om)

withSourceModel :: String -> (Handle -> IO ()) -> IO ()
withSourceModel arg = maybeWithFile ReadMode $ case arg of
    "-"  -> Nothing
    name -> Just name

maybeWithFile :: IOMode -> Maybe FilePath -> (Handle -> IO ()) -> IO ()
maybeWithFile mode (Just path) = withFile path mode
maybeWithFile mode Nothing     = case mode of
    ReadMode  -> \m -> m stdin
    WriteMode -> \m -> m stdout
    _         -> error "maybeWithFile: illegal IOMode"

render :: Pretty a => a -> Text
render = displayT . renderPretty 0.4 80 . pretty

