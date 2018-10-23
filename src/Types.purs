module Types where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over, over2, wrap)
import Data.NonEmpty (NonEmpty)
import Data.Tuple.Nested (over1)
import Pathy (AbsDir, AnyFile, RelFile, AbsFile)

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

type Input =
  { env :: EnvVars
  , flags :: Flags
  }

type EnvVars =
  { lang :: InputData
  , dir :: InputData
  , main :: InputData
  }

type Flags =
  { help :: InputData
  }

type InputData =
  { name :: String
  , description :: String
  , type_ :: String
  }

--------------------------------------------------------------------------------
-- Lifecycle
--------------------------------------------------------------------------------

data Task
  = TaskHelp
  | TaskMain Config

data Result
  = ResultHelp
  | ResultMain ResultMain

type ResultMain = Either Err DependencyGraph

data Config = Config
  { language :: Language
  , directory :: AbsDir
  , main :: RelFile
  }

type Output = Either String String

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

newtype DependencyGraph = DependencyGraph (Array Dependency)

derive instance newtypeDependencyGraph :: Newtype DependencyGraph _

instance semigroupDependencyGraph :: Semigroup DependencyGraph where
  append = over2 wrap append

instance monoidDependencyGraph :: Monoid DependencyGraph where
  mempty = wrap []

data Dependency = Dependency ModulePath ModulePath

newtype ModulePath = ModulePath (NonEmpty Array String)

newtype ModuleData = ModuleData
  { modulePath :: ModulePath
  , imports :: Array ModulePath
  }

data LangSpec = LangSpec
  { modulePathToFilePath :: ModulePath -> RelFile
  , parseModuleData :: RelFile -> SourceStr -> Either Err ModuleData
  }

newtype SourceStr = SourceStr String

newtype SourceStrDot = SourceStrDot String

data Err
  = ErrReadTextFile AbsFile
  | ErrParseModule SourceStr
  | ErrEnvLookup String
  | ErrParseLang String
  | ErrParseDir String

type PathString = String

data Language = Elm | Bla
