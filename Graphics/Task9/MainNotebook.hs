module Graphics.Task9.MainNotebook where

-- Internal
import Graphics.Task9.Types
import Graphics.Task9.Thumbnails
import Graphics.Task9.PictView
import Graphics.Task9.Utils
import Graphics.Task9.Config

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=), (:=>)), set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Display.Label      as DLabel
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Misc.EventBox      as MEventBox
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow

-- Data
import Data.Maybe (catMaybes, fromJust)
import Data.List  (elemIndex)
import qualified Data.Text as Text

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer     as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import qualified Control.Concurrent   as Conc


withNewThumbnailsWithNotebook :: Image img
                                 => LNotebook.Notebook
                                 -> (Thumbnails img -> IO a)
                                 -> Reader.ReaderT KeyMapSet IO a
withNewThumbnailsWithNotebook notebook action = do
  thumbs <- thumbnailsNew
  page <- notebook `addThumbnailsTab` thumbs
  Trans.liftIO $ do
    AWidget.widgetShowAll $ thumbsView thumbs
    LNotebook.notebookSetCurrentPage notebook page
    action thumbs

mainNotebookNew :: Task9IO LNotebook.Notebook
mainNotebookNew = do
  keymap <- notebookKeyMap <$> Reader.ask
  Trans.liftIO $ do
    notebook <- LNotebook.notebookNew
    notebook `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
      modifiers <- GEventM.eventModifier
      keyname   <- GEventM.eventKeyName
      Trans.liftIO $ do
        let Just action = lookup keyname keymap
        action notebook
    notebook `on` LNotebook.pageAdded $ \widget index -> do
      Just label <- LNotebook.notebookGetTabLabelText notebook widget
      newLabel <- MEventBox.eventBoxNew
      newLabel `set` [ AContainer.containerChild
                       :=> DLabel.labelNew (Just label :: Maybe String)]
      LNotebook.notebookSetTabLabel notebook widget newLabel
      AWidget.widgetShowAll newLabel
      widget `on` AWidget.keyPressEvent $ do
        keyname <- GEventM.eventKeyName
        Monad.when (keyname == (Text.pack "period")) $ Trans.liftIO $ do
          GGeneral.postGUIAsync $ notebookCloseChild notebook widget
        return False
      return ()
    return notebook

addThumbnailsTab :: Image img =>
                    LNotebook.Notebook -> Thumbnails img -> Task9IO Int
addThumbnailsTab notebook
  (Thumbnails { thumbsView     = thView
              , thumbsObserver = thSbj
              , thumbsModel    = thModel }) = do
  config <- Reader.ask
  Trans.liftIO $ do
    page <- notebook `LNotebook.notebookAppendPage` thView $ "thumbs"
    thSbj `Observer.addObserver` \index ->
      GGeneral.postGUIAsync $
        thumbClickAction thModel index notebook config
    return page
  where thumbClickAction store index notebook config = do
          scrolled  <- SScrolled.scrolledWindowNew Nothing Nothing
          notebook `LNotebook.notebookAppendPage` scrolled $ "pict"
          AWidget.widgetShowAll scrolled
          LNotebook.notebookSetCurrentPage notebook
           =<< fromJust <$> getTabPositionByTab notebook scrolled
          pictViewSetup scrolled store index `Reader.runReaderT` config
          AWidget.widgetShowAll scrolled
          return ()
