module Main where

import Prelude
import Types (Config(..), Dependency(..), DependencyGraph(..), Err(..), LangSpec(..), Language, ModuleData(..), ModulePath(..), Output, Result(..), SourceStr(..), SourceStrDot(..), Task(..))
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Control.Monad.Except.Trans (ExceptT, except, lift, runExceptT, throwError)
import Copy (copy, withTicks)
import Data.Array (elem, find, fromFoldable, mapMaybe, nub, (!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Enum (enumFromTo)
import Data.Function.Uncurried (runFn0)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (joinWith)
import Data.Traversable (fold, traverse)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Stats (Stats(..))
import Node.FS.Sync (readTextFile, stat)
import Node.Process (PROCESS, argv, cwd, exit, lookupEnv)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsDir, AbsFile, Path, RelFile, parseAbsDir, parseRelDir, parseRelFile, posixParser, posixPrinter, sandboxAny, unsafePrintPath, (</>))
import Specs (langSpecs)

--------------------------------------------------------------------------------
-- Constant
--------------------------------------------------------------------------------

constants :: { maxDepthLevel :: Int }
constants =
  { maxDepthLevel : 100
  }

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

      main' <-
        envLookupErr "MAIN"
        <#> (_ >>= parseAnyFileErr)
         #  wrap

      pure $ TaskMain (Config { language, directory, main : main' })

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

envLookupErr :: forall e. String -> Eff ( process :: PROCESS | e )
                                        (Either Err String)
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

run :: forall e. Either Err Task -> Eff ( fs :: FS | e) Result
run task =
  case task of
    Right TaskHelp ->
      pure $ ResultHelp
    Right (TaskMain config) ->
      runMain config
      # map (either ResultErr ResultMain)
    Left err ->
      pure $ ResultErr err

runMain :: forall e. Config -> Eff ( fs :: FS | e) (Either Err DependencyGraph)
runMain (Config { language, directory, main }) =
  runExceptT $ visit 0 langSpec directory main
  <#> case _ of (DependencyGraph xs) -> DependencyGraph (nub xs)
  where
    langSpec = langSpecs language

visit :: forall e.
         Int
         -> LangSpec
         -> AbsDir
         -> RelFile
         -> ExceptT Err (Eff ( fs :: FS | e)) DependencyGraph
visit level langSpec baseDir filePathRel = do
  when (level > constants.maxDepthLevel)
    (throwError $ ErrMaxDepthLevel constants.maxDepthLevel)

  sourceStr <-
      readTextFileErr filePathAbs
    # wrap
    # map SourceStr

  ModuleData { modulePath, imports } <-
      parseModuleData filePathRel sourceStr
    # note (ErrParseModule filePathAbs sourceStr)
    # except

  importsToFollow <-
    traverse (\modulePath -> do
                 followImport baseDir modulePath (modulePathToFilePath modulePath)
                 >>= if _ then pure (Just modulePath) else pure Nothing
             )
             imports
    <#> mapMaybe id
     #  lift

  let depsHere = DependencyGraph $ map (Dependency modulePath) importsToFollow

  depsRec <-
    traverse next importsToFollow

  pure $ (depsHere <> fold depsRec)

  where
    filePathAbs = baseDir </> filePathRel

    LangSpec { parseModuleData, modulePathToFilePath } = langSpec

    next path =
      visit (level + 1) langSpec baseDir (modulePathToFilePath path)

followImport :: forall e.
                AbsDir
                -> ModulePath
                -> RelFile
                -> Eff (fs :: FS | e) Boolean
followImport absDir modulePath relFile =
  fileExists (absDir </> relFile)

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
      copy.errors.parseLang lang ( map (langSpecs >>> unwrap >>> _.id)
                                   allLanguages
                                 )
    ErrParseDirPath path ->
      copy.errors.parseDirPath path
    ErrParseFilePath path ->
      copy.errors.parseFilePath path
    ErrParseModule path _ ->
      copy.errors.parseModule (printPath' path)
    ErrMaxDepthLevel n ->
      copy.errors.maxDepthLevel (show n)
    ErrUnknown ->
      copy.errors.unknown

outputHelp :: String
outputHelp = copy.help.title

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

handleOutput :: forall e. Output -> Eff ( console :: CONSOLE
                                        , process :: PROCESS
                                        | e
                                        )
                                        Unit
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
  <> "rankdir=LR;\n"
  <> (map f deps # joinWith "\n")
  <> "\n}"
  where
    f (Dependency a b) =
      withTicks (modulePathToStr b)
      <> " -> "
      <> withTicks (modulePathToStr a)
      <> ";"

modulePathToStr :: ModulePath -> String
modulePathToStr (ModulePath xs) = joinWith "." (fromFoldable xs)

printPath' :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
printPath' path =
  unsafePrintPath posixPrinter (sandboxAny path)

allLanguages :: Array Language
allLanguages = enumFromTo bottom top

fileExists :: forall e. AbsFile -> Eff ( fs :: FS | e) Boolean
fileExists absFile =
  printPath' absFile
  # stat
  # try
  # map (either (const false) (\(Stats statsObj) -> runFn0 statsObj.isFile))

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall e. Eff ( console :: CONSOLE
                      , process :: PROCESS
                      , fs :: FS, exception :: EXCEPTION
                      | e
                      )
                      Unit
main = do
  task <- runExceptT $ getTask
  result <- run task
  handleOutput (output result)
