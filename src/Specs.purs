module Specs where

import Prelude
import Types

import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import Pathy (file)
import Type.Prelude (SProxy(..))

langSpecs :: Language -> LangSpec
langSpecs lang =
  case lang of
    Elm -> LangSpec
      { modulePathToFilePath : \_ -> file (SProxy :: SProxy "Main.elm")
      , parseModuleData : \_ _ ->
         Right $ ModuleData
             { modulePath : ModulePath $ "Main" :| []
             , imports : []
             }
      }
    Bla -> LangSpec
      { modulePathToFilePath : \_ -> file (SProxy :: SProxy "Main.elm")
      , parseModuleData : \_ _ ->
         Right $ ModuleData
             { modulePath : ModulePath $ "Main" :| []
             , imports : []
             }
      }
