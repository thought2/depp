module Specs where

import Prelude
import Types

import Control.Alt ((<|>))
import Control.Monad.List.Trans (unfold)
import Data.Array (fromFoldable, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.List (List, stripPrefix, toUnfoldable, (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.String (Pattern(..), fromCharArray, joinWith, split)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Typelevel.Undefined (undefined)
import Data.Unfoldable (unfoldr)
import Debug.Trace (spy)
import Partial.Unsafe (unsafePartial)
import Pathy (RelFile, file, parseRelFile, posixParser)
import Text.Parsing.StringParser (Parser(..), runParser)
import Text.Parsing.StringParser.Combinators (between, lookAhead, many, optionMaybe, sepBy, sepBy1)
import Text.Parsing.StringParser.String (alphaNum, anyChar, char, noneOf, string, upperCaseChar, whiteSpace)
import Type.Prelude (SProxy(..))


langSpecs :: Language -> LangSpec
langSpecs lang =
  case lang of
    Elm -> LangSpec
      { id : "elm"
      , modulePathToFilePath : elmModulePathToFilePath
      , parseModuleData : elmParseModuleData
      }
    Bla -> LangSpec
      { id : "bla"
      , modulePathToFilePath : elmModulePathToFilePath
      , parseModuleData : elmParseModuleData
      }

--------------------------------------------------------------------------------
-- Elm
--------------------------------------------------------------------------------

elmModulePathToFilePath :: ModulePath -> RelFile
elmModulePathToFilePath (ModulePath xs) =
  fromFoldable xs
  # joinWith "/"
  # (_ <> ".elm")
  # parseRelFile posixParser
  # unsafePartial fromJust

parseModuleName :: Parser String
parseModuleName =
  (upperCaseChar <#> pure)
  <>  many (alphaNum <|> char '_')
  <#> (fromFoldable >>> fromCharArray)

parseModulePath :: Parser ModulePath
parseModulePath =
  sepBy1' parseModuleName (char '.') <#> ModulePath

parseModuleStmt :: Parser ModulePath
parseModuleStmt =
  between ((string "module" <|> string "port module") <> whiteSpace) whiteSpace parseModulePath

parseImportStmt :: Parser ModulePath
parseImportStmt =
  between (string "import" <> whiteSpace) (many (noneOf ['\n'])) parseModulePath

parseImports :: SourceStr -> Array ModulePath
parseImports (SourceStr str) =
  split (Pattern "\n") str
  # mapMaybe (runParser parseImportStmt >>> either (const Nothing) Just)

elmParseModuleData :: RelFile -> SourceStr -> Maybe ModuleData
elmParseModuleData _ sourceStr@(SourceStr str) = do
  modulePath <-
    runParser parseModuleStmt str
    # either (const Nothing) Just

  let imports = parseImports sourceStr

  Just $ ModuleData
    { modulePath
    , imports
    }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

sepBy1' :: forall a sep. Parser a -> Parser sep -> Parser (NonEmpty Array a)
sepBy1' p sep = sepBy1 p sep <#> (unsafePartial (\(x : xs) -> NonEmpty x (fromFoldable xs)))
