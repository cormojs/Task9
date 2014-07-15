{-# LANGUAGE OverloadedStrings #-}
module Main where


-- Internal
import Types
import Thumbnails

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook

-- File operation
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
