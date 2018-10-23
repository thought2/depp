module Copy where


import Data.TemplateString ((<^>))
import Data.Tuple.Nested ((/\))
import Types


-- copy ::
--   { errors ::
--        { readTextFile :: String -> String
--        , parseModuleName :: String -> String
--        , envLookup :: String -> String
--        , parseLang :: String -> String
--        , parseDir :: String -> String
--        }
--   , env ::
--        { lang :: String
--        , dir :: String
--        , main :: String
--        }
--   , help ::
--        { title :: String
--        }
--   }
-- copy =
--   { errors :
--       { readTextFile :
--           f1 "Cannot read text file '{path}'."
--              "path"
--       , parseModuleName :
--           \s -> "Something is wrong with the module name in '${s}'" <^> ["s" /\ path]
--       , envLookup :
--           \s -> "Cannot find environment variable '${s}'" <^> ["path" /\ path]
--       , parseLang :
--           \s -> "${s} is not a supported language." <^> ["lang" /\ lang]
--       , parseDir :
--           \s -> "${s} is not a valid directory path." <^> ["dir" /\ dir]
--       }
--   , env :
--       { lang : "The programming language you use in the project."
--       , dir : "Your project's source directory. Relative or absolute path."
--       , main : "The entry point of the dependency graph. Relative path to $DIR."
--       }
--   , help :
--       { title : "Provide the following environment variables"
--       }
--   }


-- f1 :: String -> String -> String
-- f1 t r1 =
--   template t <^> ["s1" /\ r1]
