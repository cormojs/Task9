module PictView where

-- Internal
import Types
import ImageStore

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), get, set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Misc.Adjustment    as MAdjustment
import qualified Graphics.UI.Gtk.Misc.EventBox      as MEventBox
import qualified Graphics.UI.Gtk.Misc.Viewport      as MViewport
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync


type PictView = SScrolled.ScrolledWindow


pictViewNewWithModel :: MV.ListStore MyImage -> Int -> (PictView -> IO ())
                        -> IO PictView
pictViewNewWithModel store index close = do
  pictView <- SScrolled.scrolledWindowNew Nothing Nothing
  adj      <- MAdjustment.adjustmentNew 0 0 0 0 0 0 
  viewport <- MViewport.viewportNew adj adj
  pictView `set` [ AContainer.containerChild := viewport ]

  indexObserver <- ObserverSync.createSub 0
  indexObserver `Observer.addObserver` \index -> do
    size <- MV.listStoreGetSize store
    if index /= (index `mod` size)
      then indexObserver `Observer.setValue` (index`mod`size)
      else pictViewMovePict index store viewport
  indexObserver `Observer.setValue` index

  pictViewSetupMouseEvent pictView close
  pictViewSetupKeyEvent pictView store indexObserver close
  return pictView

pictViewMovePict index store viewport = do
  image <- imageNewFromStore store index
  GGeneral.postGUIAsync $ do
    AContainer.containerForall viewport $ \w ->
      AContainer.containerRemove viewport w
    viewport `set` [ AContainer.containerChild := image ]
    AWidget.widgetShowAll viewport


pictViewSetupMouseEvent scrolled close = do
  scrolled `on` AWidget.buttonPressEvent $ do
    button <- GEventM.eventButton
    click <- GEventM.eventClick
    Trans.liftIO $ case click of
      GEventM.DoubleClick -> GGeneral.postGUIAsync $ close scrolled
      _ -> return ()
    return False

pictViewSetupKeyEvent scrolled store sbj close = do
  scrolled `on` AWidget.keyPressEvent $ do
    keyname <- GKeys.keyName <$> GEventM.eventKeyVal
    Trans.liftIO $ case keyname of
      "p"      -> sbj `Observer.changeValue` (subtract 1)
      "n"      -> sbj `Observer.changeValue` (+1)
      "period" -> GGeneral.postGUIAsync $ close scrolled
      _   -> putStrLn $ "Not assigned: " ++ keyname
    return False

