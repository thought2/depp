module Copy where

import Types

import Data.Monoid ((<>))
import Data.String (joinWith)
import Data.Typelevel.Undefined (undefined)

copy ::
  { errors ::
     { readTextFile :: String -> String
     , envLookup :: String -> String
     , parseLang :: String -> Array String -> String
     , parseDirPath :: String -> String
     , parseFilePath :: String -> String
     , parseModule :: String -> String
     , unknown :: String
     }
  , env ::
     { lang :: String
     , dir :: String
     , main :: String
     }
  , help ::
     { title :: String
     }
}
copy =
  { errors :
    { readTextFile : \path ->
       "Cannot read text file " <> withTicks path <> "."
    , envLookup : \env ->
       "Cannot find environment variable " <> withTicks env <> "."
    , parseLang : \lang langs ->
       lang <> " is not one of the supported languages: " <> sepByComma langs <> "."
    , parseDirPath : \path ->
       withTicks path <> " is not a valid directory path. You have to add a slash to the end."
    , parseFilePath : \path ->
       withTicks path <> " is not a valid file path."
    , parseModule : \path ->
       "Something is wrong with the module " <> withTicks path <> "."
    , unknown :
      "An unknown error occured."
    }
  , env :
    { lang : "The programming language you use in the project."
    , dir : "Your project's source directory. Relative or absolute path."
    , main : "The entry point of the dependency graph. Relative path to $DIR."
    }
  , help :
    { title : "Provide the following environment variables"
    }
  }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

withTicks :: String -> String
withTicks inner = "\"" <> inner <> "\""

sepByComma :: Array String -> String
sepByComma = joinWith ", "
