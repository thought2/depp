module Specs where

import Prelude hiding (between)
import Types (LangSpec(..), Language(..), ModuleData(..), ModulePath(..), SourceStr(..))
import Control.Alt ((<|>))
import Data.Array (fromFoldable, mapMaybe)
import Data.Either (either)
import Data.List ((:))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.String (Pattern(..), fromCharArray, joinWith, split)
import Partial.Unsafe (unsafePartial)
import Pathy (RelFile, parseRelFile, posixParser)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (between, many, sepBy1)
import Text.Parsing.StringParser.String (alphaNum, char, noneOf, string, upperCaseChar, whiteSpace)


langSpecs :: Language -> LangSpec
langSpecs lang =
  case lang of
    Elm -> LangSpec
      { id : "elm"
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
  between ((string "module" <|> string "port module") <> whiteSpace)
    whiteSpace
    parseModulePath

parseImportStmt :: Parser ModulePath
parseImportStmt =
  between (string "import" <> whiteSpace)
    (many (noneOf ['\n']))
    parseModulePath

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
