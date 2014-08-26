module Graphics.Task9.Thumbnails where

-- Internal
import Graphics.Task9.Types
import Graphics.Task9.FileView.FileImageStore
import Graphics.Task9.Utils
import Graphics.Task9.Config


-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on)

import qualified Graphics.UI.Gtk.Abstract.Object    as AObject
import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.General.Enums      as GEnums
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf

-- System
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import System.FilePath     ((</>))

-- Control
import qualified Control.Monad        as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import Control.Applicative ((<$>))
import qualified Control.Monad.Trans  as Trans

-- Data
import Data.List (sort)

type StoreKeyMap image = KeyMap (AWidget.Widget, MV.ListStore image, Int)

thumbnailsNew :: Image image =>
                 Reader.ReaderT KeyMapSet IO (Thumbnails image)
thumbnailsNew = do
  keymap <- Reader.ask
  Trans.liftIO $ do
    imgStore <- imageStoreNew
  
    imgView <- MV.iconViewNewWithModel imgStore
    imgView `set` [ MV.iconViewPixbufColumn := MV.makeColumnIdPixbuf 1
                  , MV.iconViewTextColumn   := MV.makeColumnIdString 0 ]
    AObject.castToObject imgView `on` AObject.objectDestroy $ do
      MV.listStoreClear imgStore
  
    subject <- ObserverSync.createSub (0 :: Int)
    imgView `on` MV.itemActivated $ \[index] -> do
      subject `Observer.setValue` index
    scrolled <- SScrolled.scrolledWindowNew Nothing Nothing
    scrolled `set` [ AContainer.containerChild := imgView ]
    scrolled `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
      iters <- Trans.liftIO $ MV.iconViewGetSelectedItems imgView
      keyname <- GEventM.eventKeyName
      let Just action = lookup keyname $ imageKeyMap keymap
      Trans.liftIO $ case iters of
        [[index]] -> action (AWidget.toWidget scrolled,
                             imgStore, index)
  
    return Thumbnails { thumbsModel    = imgStore
                      , thumbsView     = scrolled
                      , thumbsObserver = subject }
  where
    compareRow :: Ord b => MV.ListStore a -> (a -> b)
                  -> MV.TreeIter -> MV.TreeIter -> IO Ordering
    compareRow store acc iter1 iter2 = do
      v1 <- MV.treeModelGetRow store iter1
      v2 <- MV.treeModelGetRow store iter2
      return $ acc v1 `compare` acc v2
  
