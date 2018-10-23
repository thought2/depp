module Main where

import Prelude
import Types

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Control.Monad.Except.Trans (ExceptT(..), except, lift, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (under, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Symbol (SProxy(..))
import Data.Traversable (fold, traverse)
import Data.Typelevel.Undefined (undefined)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readFile, readTextFile)
import Node.Process (PROCESS, lookupEnv)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsFile, AnyDir, AnyFile, AnyPath, Path, RelFile, AbsDir, file, parseAbsDir, parseRelDir, posixParser, posixPrinter, printPath, sandboxAny, unsafePrintPath, (</>))
import Specs (langSpecs)


--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

getTask :: forall e. Eff (process :: PROCESS | e) Task
--getTask :: forall e. ExceptT Err (Eff (process :: PROCESS | e)) Task
getTask = undefined
  -- do
  -- help <- argv <#> (_ >>= _ !! 1 >>> )
  -- language <- errEnvLookup "LANG" <#> (_ >>= errReadLang) # wrap
  -- directory <- errEnvLookup "DIR" <#> (_ >>= errParseAnyDir) # wrap

  -- pure $ if help then
  --   ConfigHelp else
  --   ConfigRun (Config { language, directory }


--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

readTextFileErr :: forall e. AbsFile -> Eff ( fs :: FS | e ) (Either Err String)
readTextFileErr absFilePath =
      readTextFile UTF8 (printPath' absFilePath)
   #  try
  <#> lmap (const $ ErrReadTextFile absFilePath)

--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

run :: Task -> Eff _ Result
run task =
  case task of
    TaskHelp -> pure $ ResultHelp
    TaskMain config -> runMain config <#> ResultMain

runMain :: Config -> Eff _ (Either Err DependencyGraph)
runMain (Config { language, directory, main }) =
  visit langSpec directory main
  where
    langSpec = langSpecs language

visit :: LangSpec -> AbsDir -> RelFile -> Eff _ (Either Err DependencyGraph)
visit langSpec baseDir filePathRel = runExceptT $ do
  let LangSpec { parseModuleData, modulePathToFilePath } = langSpec
  sourceStr <- readTextFileErr filePathAbs # wrap # map SourceStr
  ModuleData { modulePath, imports } <- parseModuleData filePathRel sourceStr # except
  let depsHere = DependencyGraph $ map (Dependency modulePath) imports

  depsRec <- traverse (modulePathToFilePath >>> visit langSpec baseDir >>> wrap) imports

  pure $ (depsHere <> fold depsRec)
  where
    filePathAbs = baseDir </> filePathRel

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

output :: Result -> Output
output result =
  case result of
    ResultHelp -> Right outputHelp
    ResultMain (Right deps) -> case getDot deps of SourceStrDot str -> Right str
    ResultMain (Left err) -> Left $ outputError err

outputError :: Err -> String
outputError = undefined

outputHelp :: String
outputHelp = undefined

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

printOutput :: Output -> Eff _ Unit
printOutput output =
  case output of
    Left str -> error str
    Right str -> log str

getDot :: DependencyGraph -> SourceStrDot
getDot = undefined

printPath' :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
printPath' path =
  unsafePrintPath posixPrinter (sandboxAny path)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
  task <- getTask
  result <- run task
  printOutput (output result)
