{-# LANGUAGE OverloadedStrings #-}
module Main where


-- Internal
import Types
import Thumbnails
import PictView

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Misc.EventBox      as MEventBox
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow

-- File operation
import System.Environment  (getArgs)

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import qualified Control.Concurrent   as Conc

-- Data
import Data.Maybe (catMaybes, fromJust)
import Data.List  (elemIndex)


main :: IO ()
main = do
  GGeneral.initGUI
  notebook <- LNotebook.notebookNew

  thumbs <- thumbnailsNew
  notebook `addThumbnailsTab` thumbs
  
  window <- WWindow.windowNew
  window `set` [ WWindow.windowDefaultWidth  := 850
               , WWindow.windowDefaultHeight := 600
               , AContainer.containerChild   := notebook ]
  window `on` AWidget.deleteEvent $ Trans.liftIO GGeneral.mainQuit >> return False
  AWidget.widgetShowAll window
  
  Conc.forkOS $
    Monad.mapM_ (populateStoreFromFileOrDir (thumbsModel thumbs)) =<< getArgs

  GGeneral.mainGUI


addThumbnailsTab notebook
  (Thumbnails { thumbsView     = thView
              , thumbsObserver = thSbj
              , thumbsModel    = thModel }) = do
  notebook `LNotebook.notebookAppendPage` thView $ "thumbs"
  thSbj `Observer.addObserver` \index ->
    GGeneral.postGUIAsync $ do
      pict <- pictViewNewWithModel thModel index $ closeTab notebook
      -- pict' <- pictViewNew filename $ closeTab notebook
      notebook `LNotebook.notebookAppendPage` pict $ "pict"
      AWidget.widgetShowAll pict
      LNotebook.notebookSetCurrentPage notebook
        =<< fromJust <$> getTabPositionByTab notebook pict

-- original -> mikutter/core/mui/gtk_txtension.rb
getTabPositionByTab :: (LNotebook.NotebookClass notebook,
                        AWidget.WidgetClass widget) =>
                       notebook -> widget -> IO (Maybe Int)
getTabPositionByTab notebook label = do
  numPages <- LNotebook.notebookGetNPages notebook
  pages <- catMaybes <$> Monad.mapM (LNotebook.notebookGetNthPage notebook) [0..numPages]
  return $ (AWidget.toWidget label) `elemIndex` pages

closeTab notebook page = do
  Just n <- getTabPositionByTab notebook page
  LNotebook.notebookRemovePage notebook n
