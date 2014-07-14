{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Gtk

import qualified Graphics.UI.Gtk.General.Enums   as GE
import qualified Graphics.UI.Gtk.General.General as GG
import qualified Graphics.UI.Gtk.Windows.Window  as WW
import qualified Graphics.UI.Gtk.ModelView       as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SS
import qualified Graphics.UI.Gtk.Gdk.Pixbuf      as GP

import qualified System.Directory as Dir
import qualified System.IO        as IO
import qualified System.FilePath  as FilePath

import System.FilePath     ((</>))
import Control.Applicative ((<$>))

import qualified Control.Monad as Monad

import Control.Monad.Trans (liftIO)



main :: IO ()
main = do
  GG.initGUI
  
  imgView <- samplePixbufView1

  scrolled <- SS.scrolledWindowNew Nothing Nothing
  scrolled `set` [ containerChild := imgView ]

  window <- WW.windowNew
  window `set` [ WW.windowDefaultWidth  := 850
               , WW.windowDefaultHeight := 600
               , containerChild         := scrolled ]
  window `on` deleteEvent $ liftIO GG.mainQuit >> return False
  widgetShowAll window
  
  GG.mainGUI

type MyImage = (IO.FilePath, Pixbuf)

samplePixbufView1 :: IO MV.IconView
samplePixbufView1 = do
  imgStore <- setupStore
  populateStore imgStore

  imgStoreSorted <- MV.treeModelSortNewWithModel imgStore
  MV.treeSortableSetSortFunc imgStoreSorted 1 (compareRow imgStore fst)
  MV.treeSortableSetSortColumnId imgStoreSorted 1 GE.SortDescending

  imgView <- MV.iconViewNewWithModel imgStoreSorted
  imgView `set` [ MV.iconViewModel := Just imgStoreSorted
                , MV.iconViewPixbufColumn := MV.makeColumnIdPixbuf 1
                , MV.iconViewTextColumn := MV.makeColumnIdString 0 ]
  return imgView
  where
    -- store(model)を作成
    setupStore :: IO (MV.ListStore MyImage)
    setupStore = do
      store <- MV.listStoreNew ([] :: [MyImage])
      MV.customStoreSetColumn store (MV.makeColumnIdString 0) (FilePath.takeFileName . fst)
      MV.customStoreSetColumn store (MV.makeColumnIdPixbuf 1) snd
      return store
    -- storeにMyImageを流し込む
    populateStore store = do
      let dir = "/Users/cormo/Pictures/yuri/"
          isPict f = or $ map (FilePath.takeExtension f == ) [".jpg", ".jpeg", ".png"]
      files <-
        Monad.filterM Dir.doesFileExist
        =<< (map (dir</>) <$> Dir.getDirectoryContents dir)
      Monad.forM_ (filter isPict files) $ \filename -> do
        pixbuf <- GP.pixbufNewFromFileAtSize filename 240 300
        store `MV.listStoreAppend` (filename, pixbuf)
    compareRow :: Ord b => MV.ListStore a -> (a -> b) -> TreeIter -> TreeIter -> IO Ordering
    compareRow store acc iter1 iter2 = do
      v1 <- MV.treeModelGetRow store iter1
      v2 <- MV.treeModelGetRow store iter2
      return $ (acc v1) `compare` (acc v2)

