module Specs where

import Prelude
import Types

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Pathy (file)
import Type.Prelude (SProxy(..))

langSpecs :: Language -> LangSpec
langSpecs lang =
  case lang of
    Elm -> LangSpec
      { id : "elm"
      , modulePathToFilePath : \_ -> file (SProxy :: SProxy "Main.elm")
      , parseModuleData : \_ _ ->
         Just $ ModuleData
             { modulePath : ModulePath $ "Main" :| []
             , imports : []
             }
      }
    Bla -> LangSpec
      { id : "bla"
      , modulePathToFilePath : \_ -> file (SProxy :: SProxy "Main.elm")
      , parseModuleData : \_ _ ->
         Just $ ModuleData
             { modulePath : ModulePath $ "Main" :| []
             , imports : []
             }
      }
