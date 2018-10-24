module Main where

import Prelude
import Types

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT(..), except, lift, runExceptT, throwError)
import Control.Monad.Except.Trans as T
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Copy (copy)
import Data.Array (elem, find, head, (!!))
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), either, note)
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (under, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (fold, traverse)
import Data.Typelevel.Undefined (undefined)
import Debug.Trace (spy)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readFile, readTextFile)
import Node.Process (PROCESS, argv, cwd, exit, lookupEnv)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsDir, AbsFile, AnyDir, AnyFile, AnyPath, Path, RelFile, file, parseAbsDir, parseRelDir, parseRelFile, posixParser, posixPrinter, printPath, sandboxAny, unsafePrintPath, (</>))
import Specs (langSpecs)


--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

getTask :: forall e. ExceptT Err (Eff (process :: PROCESS | e)) Task
getTask = do
  help <-
    argv
    # map parseArgs
    # wrap

  if help
    then pure TaskHelp
    else do
      language <-
        envLookupErr "LANGUAGE"
        <#> (_ >>= parseLangErr)
         #  wrap

      cwd' <-
        cwd
        <#> (_ <> "/")
        >>> parseAbsDir posixParser
        >>> note ErrUnknown
         #  wrap

      directory <-
        envLookupErr "DIR"
        <#> (_ >>= parseAnyDirErr cwd')
         #  wrap

      main <-
        envLookupErr "MAIN"
        <#> (_ >>= parseAnyFileErr)
         #  wrap

      pure $ TaskMain (Config { language, directory, main })

parseArgs :: Array String -> Either Err Boolean
parseArgs xs =
    (xs !! 2)
  # maybe false (_ `elem` ["--help", "-h"])
  # Right

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

readTextFileErr :: forall e. AbsFile -> Eff ( fs :: FS | e ) (Either Err String)
readTextFileErr absFilePath =
  readTextFile UTF8 (printPath' absFilePath)
   #  try
  <#> lmap (const $ ErrReadTextFile absFilePath)

envLookupErr :: forall e. String -> Eff ( process :: PROCESS | e ) (Either Err String)
envLookupErr env =
  lookupEnv env
  <#> note (ErrEnvLookup env)

parseLangErr :: String -> Either Err Language
parseLangErr str =
  find (langSpecs >>> unwrap >>> _.id >>> (_ == str)) allLanguages
  # note (ErrParseLang str)

parseAnyDirErr :: AbsDir -> String -> Either Err AbsDir
parseAnyDirErr baseDir dir =
  parseAbsDir posixParser dir
  <|> (parseRelDir posixParser dir <#> (baseDir </> _))
   #  note (ErrParseDirPath dir)

parseAnyFileErr :: String -> Either Err RelFile
parseAnyFileErr str =
  parseRelFile posixParser str
  # note (ErrParseFilePath str)

--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

run :: Either Err Task -> Eff _ Result
run task =
  case task of
    Right TaskHelp -> pure $ ResultHelp
    Right (TaskMain config) -> runMain config # map (either ResultErr ResultMain)
    Left err -> pure $ ResultErr err

runMain :: Config -> Eff _ (Either Err DependencyGraph)
runMain (Config { language, directory, main }) =
  runExceptT $ visit langSpec directory main
  where
    langSpec = langSpecs language

visit :: LangSpec -> AbsDir -> RelFile -> ExceptT Err (Eff _) DependencyGraph
visit langSpec baseDir filePathRel = do
  let LangSpec { parseModuleData, modulePathToFilePath } = langSpec

  sourceStr <-
      readTextFileErr filePathAbs
    # wrap
    # map SourceStr

  ModuleData { modulePath, imports } <-
      parseModuleData filePathRel sourceStr
    # note (ErrParseModule filePathAbs sourceStr)
    # except

  let depsHere = DependencyGraph $ map (Dependency modulePath) imports

  depsRec <-
    traverse (modulePathToFilePath >>> visit langSpec baseDir) imports

  pure $ (depsHere <> fold depsRec)

  where
    filePathAbs = baseDir </> filePathRel

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

output :: Result -> Output
output result =
  case result of
    ResultHelp ->
      Right outputHelp
    ResultMain deps ->
      case getDot deps of SourceStrDot str -> Right str
    ResultErr err ->
      Left $ outputError err

outputError :: Err -> String
outputError err =
  case err of
    ErrReadTextFile path ->
      copy.errors.readTextFile (printPath' path)
    ErrEnvLookup env ->
      copy.errors.envLookup env
    ErrParseLang lang ->
      copy.errors.parseLang lang (map (langSpecs >>> unwrap >>> _.id) allLanguages)
    ErrParseDirPath path ->
      copy.errors.parseDirPath path
    ErrParseFilePath path ->
      copy.errors.parseFilePath path
    ErrParseModule path _ ->
      copy.errors.parseModule (printPath' path)
    ErrUnknown ->
      copy.errors.unknown

outputHelp :: String
outputHelp = copy.help.title

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

handleOutput :: forall e. Output -> Eff ( console :: CONSOLE, process :: PROCESS | e) Unit
handleOutput output =
  case output of
    Left str -> do
      error str
      exit 1
    Right str -> do
      log str
      exit 0

getDot :: DependencyGraph -> SourceStrDot
getDot (DependencyGraph deps) = SourceStrDot $
  "digraph {\n"
  <> (map f deps # joinWith "\n")
  <> "}"
  where
    f (Dependency a b) =
      modulePathToStr a
      <> " -- "
      <> modulePathToStr b

modulePathToStr :: ModulePath -> String
modulePathToStr = undefined

printPath' :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
printPath' path =
  unsafePrintPath posixPrinter (sandboxAny path)

allLanguages :: Array Language
allLanguages = enumFromTo bottom top

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
  task <- runExceptT $ getTask
  result <- run task
  handleOutput (output result)
