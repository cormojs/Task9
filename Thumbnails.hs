module Thumbnails where

-- Internal
import Types
import ImageStore

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.General.Enums      as GEnums
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf

-- System
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import System.FilePath     ((</>))

-- Control
import qualified Control.Monad       as Monad
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import Control.Applicative ((<$>))

-- Data
import Data.List (sort)


thumbnailsNew :: IO Thumbnails
thumbnailsNew = do
  imgStore <- imageStoreNew

  imgStoreSorted <- MV.treeModelSortNewWithModel imgStore
  MV.treeSortableSetSortFunc imgStoreSorted 1 (compareRow imgStore imageFilename)
  MV.treeSortableSetSortColumnId imgStoreSorted 1 GEnums.SortDescending

  imgView <- MV.iconViewNewWithModel imgStore
  imgView `set` [ MV.iconViewPixbufColumn := MV.makeColumnIdPixbuf 1
                , MV.iconViewTextColumn   := MV.makeColumnIdString 0 ]

  subject <- ObserverSync.createSub (0 :: Int)
  imgView `on` MV.itemActivated $ \[index] -> do
    subject `Observer.setValue` index

  scrolled <- SScrolled.scrolledWindowNew Nothing Nothing
  scrolled `set` [ AContainer.containerChild := imgView ]

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
