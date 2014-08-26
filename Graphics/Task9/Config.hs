{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Task9.Config where

-- internal
import Graphics.Task9.Types
import Graphics.Task9.Utils

-- Nijie
import Web.Nijie
import Web.Nijie.Types
import Web.Nijie.Login

-- gtk
import Graphics.UI.Gtk (AttrOp((:=), (:=>)), set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Widget  as AWidget
import qualified Graphics.UI.Gtk.Windows.Window   as WWindow
import qualified Graphics.UI.Gtk.ModelView        as MV
import qualified Graphics.UI.Gtk.Layout.Notebook  as LNotebook

-- observer
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync

-- control.*
import qualified Control.Monad as Monad
import Control.Applicative ((<$>), (<*>))
import qualified Control.Concurrent   as Conc

-- Text
import Data.Text (Text)
import qualified Data.Text as Text

-- data
import qualified Data.List as List
import Data.Maybe (fromJust)

data Config = Config { keyConfigSet :: KeyMapSet }

type NotebookStore a = (LNotebook.Notebook, MV.ListStore a, Int)


-- data NamedAction a =
--   NamedAction { actionName  :: Text
--               , namedAction :: (a -> IO ()) }

-- instance Eq (NamedAction a) where
--   (==) (NamedAction a _) (NamedAction b _) = a == b

-- instance Show (NamedAction a) where
--   show (NamedAction name _) = "NamedAction " ++ Text.unpack name

-- class NamedActions a where
--   namedActions :: [NamedAction a]


-- defaultKeyConfigSet =
--   KeyMapSet { parentConfig   = [ KeyMap "L" "next_tab"
--                                   , KeyMap "H" "prev_tab" ]
--                , pictViewConfig = []
--                , njeViewConfig  = []
--                }
--   where go str = fromJust $ List.find ((str==).actionName) namedActions

-- --- instances of NamedActions
-- instance NamedActions LNotebook.Notebook where
--   namedActions = [ NamedAction "next_tab" LNotebook.notebookNextPage
--                  , NamedAction "prev_tab" LNotebook.notebookPrevPage ]

-- instance NamedActions Pict where
--   namedActions = [ NamedAction "next_pict_main" $ \p ->
--                     main p`Observer.changeValue` (+1)
--                  , NamedAction "prev_pict_main" $ \p ->
--                     main p`Observer.changeValue` (subtract 1)
--                  , NamedAction "next_pict_sub" $ \p ->
--                     sub p`Observer.changeValue` (+1)
--                  , NamedAction "prev_pict_sub" $ \p ->
--                     sub p`Observer.changeValue` (subtract 1) ]
--     where main = pictMainState
--           sub  = pictSubState

-- instance NamedActions (NotebookStore NijiePage) where
--   namedActions = [
--     NamedAction "nje_open_favs" $ \(notebook, store, index) -> do
--        withNewThumbnailsWithNotebook notebook $ \thumbs ->
--          Conc.forkOS $ do
--            populateStoreFromNijieThumbs (NjeFav 1) $ thumbsModel thumbs
--            populateStoreFromNijieThumbs (NjeFav 2) $ thumbsModel thumbs
--            return ()
--        return ()
--     ]

