{-# LANGUAGE OverloadedStrings #-}
module Main where


-- Private
import Types

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.General.Enums      as GEnums
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook

-- File operation
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath

import System.FilePath     ((</>))
import System.Environment  (getArgs)

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as Trans


main :: IO ()
main = do
  GGeneral.initGUI
  
  Thumbnails { thumbsView  = thView
             , thumbsModel = thModel } <- thumbnailsNew
  Monad.mapM_ (populateStoreFromDir thModel) =<< getArgs

  notebook <- LNotebook.notebookNew
  notebook `LNotebook.notebookAppendPage` thView $ "thumbs"

  window <- WWindow.windowNew
  window `set` [ WWindow.windowDefaultWidth  := 850
               , WWindow.windowDefaultHeight := 600
               , AContainer.containerChild   := notebook ]
  window `on` AWidget.deleteEvent $ Trans.liftIO GGeneral.mainQuit >> return False
  AWidget.widgetShowAll window
  
  GGeneral.mainGUI

populateStoreFromDir store dir = do
  let isPict f = FilePath.takeExtension f `elem` [".jpg", ".jpeg", ".png"]
  files <-
    Monad.filterM Dir.doesFileExist
    =<< (map (dir</>) <$> Dir.getDirectoryContents dir)
  Monad.forM_ (filter isPict files) $ \filename -> do
    pixbuf <- GPixbuf.pixbufNewFromFileAtSize filename 240 300
    store `MV.listStoreAppend` (filename, pixbuf)

thumbnailsNew :: IO Thumbnails
thumbnailsNew = do
  imgStore <- setupStore

  imgStoreSorted <- MV.treeModelSortNewWithModel imgStore
  MV.treeSortableSetSortFunc imgStoreSorted 1 (compareRow imgStore fst)
  MV.treeSortableSetSortColumnId imgStoreSorted 1 GEnums.SortDescending

  imgView <- MV.iconViewNewWithModel imgStoreSorted
  imgView `set` [ MV.iconViewModel        := Just imgStoreSorted
                , MV.iconViewPixbufColumn := MV.makeColumnIdPixbuf 1
                , MV.iconViewTextColumn   := MV.makeColumnIdString 0 ]

  scrolled <- SScrolled.scrolledWindowNew Nothing Nothing
  scrolled `set` [ AContainer.containerChild := imgView ]

  return Thumbnails { thumbsModel = imgStore, thumbsView = scrolled }
  where
    -- store(model)を作成
    setupStore :: IO (MV.ListStore MyImage)
    setupStore = do
      store <- MV.listStoreNew ([] :: [MyImage])
      MV.customStoreSetColumn store (MV.makeColumnIdString 0) (FilePath.takeFileName . fst)
      MV.customStoreSetColumn store (MV.makeColumnIdPixbuf 1) snd
      return store
    compareRow :: Ord b => MV.ListStore a -> (a -> b)
                  -> MV.TreeIter -> MV.TreeIter -> IO Ordering
    compareRow store acc iter1 iter2 = do
      v1 <- MV.treeModelGetRow store iter1
      v2 <- MV.treeModelGetRow store iter2
      return $ acc v1 `compare` acc v2

