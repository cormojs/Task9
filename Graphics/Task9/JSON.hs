{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Task9.JSON where

-- internal
import Graphics.Task9.Config

-- aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)

-- control.*
import qualified Control.Monad as Monad
import Control.Applicative ((<$>), (<*>))

-- list
import qualified Data.List as List



-- JSON instances
$(deriveJSON defaultOptions ''Config)
$(deriveJSON defaultOptions ''KeyConfigSet)


instance (NamedActions a) => Aeson.ToJSON (KeyConfig a) where
  toJSON (KeyConfig key action) = Aeson.object [ "keyname" .= key
                                               , "action"  .= action ]

instance (NamedActions a) => Aeson.FromJSON (KeyConfig a) where
  parseJSON (Aeson.Object v) = KeyConfig
                               <$> v .: "keyname"
                               <*> v .: "action"
  parseJSON _ = Monad.mzero


instance (NamedActions a) => Aeson.FromJSON (NamedAction a)
 where
   parseJSON (Aeson.String text) = case find' text namedActions of
     Just action -> return $ action
     Nothing  -> Monad.mzero
     where find' text = List.find ((text==).actionName) 
   parseJSON _ = Monad.mzero

instance Aeson.ToJSON (NamedAction a) where
  toJSON (NamedAction name _) = Aeson.toJSON name
