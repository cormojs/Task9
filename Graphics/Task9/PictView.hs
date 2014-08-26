module Graphics.Task9.PictView where

-- Internal
import Graphics.Task9.Types
import Graphics.Task9.FileView.FileImageStore
import Graphics.Task9.Utils

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=), (:~), (:=>)), get, set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Bin       as ABin
import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Range     as ARange
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Gdk.DrawWindow     as GDrawWindow
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Misc.Adjustment    as MAdjustment
import qualified Graphics.UI.Gtk.Misc.EventBox      as MEventBox
import qualified Graphics.UI.Gtk.Misc.Viewport      as MViewport
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow

-- Control
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import qualified Control.Concurrent   as Conc

-- Data.*
import qualified Data.List  as List
import qualified Data.Maybe as Maybe


pictViewMovePict index store subStore
  (Pict { pictWindow = window
        , pictSubState  = subObserver }) = do
    notebookSetChildTitle window "loading..."
    img <- store `MV.listStoreGetValue` index
    action <- imageThumbAction img
    MV.listStoreClear subStore
    case action of
      SinglePageAction -> do
        subStore `MV.listStoreAppend` img
        subObserver `Observer.setValue` 0
      AnotherThumbnailsAction populate -> do
        populate subStore
        subObserver `Observer.setValue` 0


pictViewMoveSubPict subIndex subStore dImage
  pict@(Pict { pictWindow = window
        , pictResizeState = state }) = do
    viewport <- MViewport.castToViewport
                <$> Maybe.fromJust <$> ABin.binGetChild window
    pictResizeCurrent subStore viewport dImage pict
    img    <- subStore `MV.listStoreGetValue` subIndex
 
    size <- MV.listStoreGetSize subStore
    let label = List.delete '\n' (imageName img) ++ " "
                ++ "[" ++ show (subIndex+1) ++ "/"
                ++ show size ++ "]"
    getParentWindow window
      >>= (`set` [ WWindow.windowTitle := label ])
    notebookSetChildTitle window label
    where getParentWindow :: AWidget.WidgetClass self =>
                             self -> IO WWindow.Window
          getParentWindow child =
            WWindow.castToWindow <$> AWidget.widgetGetToplevel child

pictViewSetupMouseEvent :: Pict -> GEventM.EventM GEventM.EButton Bool
pictViewSetupMouseEvent (Pict { pictWindow    = window
                              , pictMainState = indexObserver
                              , pictSubState  = subObserver }) = do
  button <- GEventM.eventButton
  click <- GEventM.eventClick
  Trans.liftIO $ case click of
    GEventM.DoubleClick
      -> GGeneral.postGUIAsync $ notebookFindAndCloseChild window
    _ -> return ()
  return False


pictSubChange  = Observer.changeValue . pictSubState
pictMainChange = Observer.changeValue . pictMainState

pictVScroll update (Pict { pictWindow = window }) = do
  maybar <- SScrolled.scrolledWindowGetVScrollbar window
  case maybar of
    Just bar -> bar `set` [ ARange.rangeValue :~ update ]
    Nothing -> return ()

pictHScroll update (Pict { pictWindow = window }) = do
  maybar <- SScrolled.scrolledWindowGetHScrollbar window
  case maybar of
    Just bar -> bar `set` [ ARange.rangeValue :~ update ]
    Nothing -> return ()

pictToggleResizeState (Pict { pictResizeState = state }) =
  state `Observer.changeValue` circularSucc
  where circularSucc n | n == maxBound = minBound
                       | otherwise     = succ n


pictRotate (Pict { pictRotateState = s }) = s `Observer.changeValue` circ
  where
    circ GPixbuf.PixbufRotateNone             = GPixbuf.PixbufRotateCounterclockwise
    circ GPixbuf.PixbufRotateCounterclockwise = GPixbuf.PixbufRotateUpsidedown
    circ GPixbuf.PixbufRotateUpsidedown       = GPixbuf.PixbufRotateClockwise
    circ GPixbuf.PixbufRotateClockwise        = GPixbuf.PixbufRotateNone

pictRotateInverse (Pict { pictRotateState = s }) = s `Observer.changeValue` circ
  where
    circ GPixbuf.PixbufRotateNone             = GPixbuf.PixbufRotateClockwise
    circ GPixbuf.PixbufRotateClockwise        = GPixbuf.PixbufRotateUpsidedown
    circ GPixbuf.PixbufRotateUpsidedown       = GPixbuf.PixbufRotateCounterclockwise
    circ GPixbuf.PixbufRotateCounterclockwise = GPixbuf.PixbufRotateNone

pictResizeCurrent subStore viewport dImage
  (Pict { pictSubState = subObserver
        , pictResizeState = resizeObserver
        , pictRotateState = rotateObserver }) = do
  index  <- Observer.getValue subObserver
  rotate <- Observer.getValue rotateObserver
  resize <- Observer.getValue resizeObserver

  pixbuf <- imageFull =<< subStore `MV.listStoreGetValue` index
  pixbuf' <- GPixbuf.pixbufRotateSimple pixbuf rotate
  

  view <- MViewport.viewportGetViewWindow viewport
  w <- GDrawWindow.drawWindowGetWidth  view
  h <- GDrawWindow.drawWindowGetHeight view
  dImage `set` [ DImage.imagePixbuf
                 :=> resizePixbuf pixbuf' resize (w, h) ]

pictViewSetup :: Image img =>
                 SScrolled.ScrolledWindow ->
                 MV.ListStore img -> Int ->
                 Reader.ReaderT KeyMapSet IO ()
pictViewSetup window store index = do
  config <- Reader.ask
  Trans.liftIO $ do
    subStore <- MV.listStoreNew ([] :: [image])
    adj      <- MAdjustment.adjustmentNew 0 0 0 0 0 0
    viewport <- MViewport.viewportNew adj adj
    dImage   <- DImage.imageNew
   
    viewport `set` [ AContainer.containerChild := dImage ]
    window `set` [ AContainer.containerChild := viewport ]
   
    indexObserver <- ObserverSync.createSub 0
    subObserver   <- ObserverSync.createSub 0
    resizeObserver <- ObserverSync.createSub NoResize
    rotateObserver <- ObserverSync.createSub GPixbuf.PixbufRotateNone
   
    let pict = Pict { pictWindow = window
                    , pictMainState = indexObserver
                    , pictSubState  = subObserver
                    , pictResizeState = resizeObserver
                    , pictRotateState = rotateObserver }
   
    indexObserver `Observer.addObserver` \index -> do
      size <- MV.listStoreGetSize store
      if index == (index `mod` size)
        then pictViewMovePict index store subStore pict
        else notebookFindAndCloseChild window
    subObserver `Observer.addObserver` \subIndex -> do
      size <- MV.listStoreGetSize subStore
      let go subIndex size
            | subIndex == (subIndex `mod` size) =
              pictViewMoveSubPict subIndex subStore dImage pict
            | subIndex < 0 =
              indexObserver `Observer.changeValue` subtract 1
            | otherwise    =
              indexObserver `Observer.changeValue` (+1)
      Monad.when (size /= 0) $ go subIndex size
    resizeObserver `Observer.addObserver` \resize ->
      pictResizeCurrent subStore viewport dImage pict
    rotateObserver `Observer.addObserver` \_ ->
      resizeObserver `Observer.changeValue` id


    indexObserver `Observer.setValue` index
   
    window `on` AWidget.buttonPressEvent $
      pictViewSetupMouseEvent pict
 
    window `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
      keyname <- GEventM.eventKeyName
      let Just action = lookup keyname $ pictViewKeyMap config
      Trans.liftIO $ action pict

    window `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
      keyname <- GEventM.eventKeyName
      subIndex <- Trans.liftIO $ Observer.getValue subObserver
      let Just action = lookup keyname $ imageKeyMap config
      Trans.liftIO $ action (AWidget.toWidget window,
                             subStore, subIndex)
    return ()
