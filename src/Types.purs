module Types where

import Prelude

import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
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
  | ResultMain DependencyGraph
  | ResultErr Err

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

newtype LangSpec = LangSpec
  { id :: String
  , modulePathToFilePath :: ModulePath -> RelFile
  , parseModuleData :: RelFile -> SourceStr -> Maybe ModuleData
  }

derive instance newtypeLangSpec :: Newtype LangSpec _

newtype SourceStr = SourceStr String

newtype SourceStrDot = SourceStrDot String

data Err
  = ErrReadTextFile AbsFile
  | ErrEnvLookup String
  | ErrParseLang String
  | ErrParseDirPath String
  | ErrParseFilePath String
  | ErrParseModule AbsFile SourceStr
  | ErrUnknown

type PathString = String

data Language = Elm | Bla

derive instance genericLanguage :: Generic Language _

instance showLanguage :: Show Language where
  show = genericShow

instance eqLanguage :: Eq Language where
  eq = genericEq

instance enumLanguage :: Enum Language where
  succ = genericSucc
  pred = genericPred

instance ordLanguage :: Ord Language where
  compare = genericCompare

instance boundedLanguage :: Bounded Language where
  bottom = genericBottom
  top = genericTop
