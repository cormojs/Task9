module Graphics.Task9.Utils where

-- internal
import Graphics.Task9.Types

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=), (:=>)), set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Display.Label      as DLabel
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Misc.EventBox      as MEventBox
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow


-- Data
import Data.Maybe (catMaybes, fromJust)
import Data.List  (elemIndex)

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader



-- Pixbuf utils


resizePixbuf :: GPixbuf.Pixbuf
                -> ResizeState
                -> (Int, Int)
                -> IO GPixbuf.Pixbuf
resizePixbuf pixbuf NoResize _ = return pixbuf
resizePixbuf pixbuf state (w, h) = do
  pW <- GPixbuf.pixbufGetWidth  pixbuf
  pH <- GPixbuf.pixbufGetHeight pixbuf
  let (w', h') = go state (w, h) (pW, pH)
  pixbuf' <- GPixbuf.pixbufScaleSimple pixbuf w' h' GPixbuf.InterpBilinear
  return pixbuf'
  where
    asp a b = fromIntegral a / fromIntegral b
    go ResizeInside (w, h) (pW, pH)
      | w >= pW && h >= pH  = (pW, pH)
      | w`asp`h < pW`asp`pH = (w, round $
                                  fromIntegral w * (pH`asp`pW))
      | otherwise           = (round $ fromIntegral h * (pW`asp`pH),
                               h)
    go ResizeOutside (w, h) (pW, pH)
      | w >= pW && h >= pH  = (pW, pH)
      | w`asp`h < pW`asp`pH = min (pW, pH) (round $ fromIntegral h * (pW`asp`pH),
                                            h)
      | otherwise           = min (pW, pH) (w, round $
                                               fromIntegral w * (pH`asp`pW))
        

-- Notebook utils
getTabPositionByTab :: (LNotebook.NotebookClass notebook,
                        AWidget.WidgetClass widget) =>
                       notebook -> widget -> IO (Maybe Int)
getTabPositionByTab notebook label = do
  numPages <- LNotebook.notebookGetNPages notebook
  pages <- catMaybes
           <$> Monad.mapM (LNotebook.notebookGetNthPage notebook)
                [0..numPages]
  return $ AWidget.toWidget label `elemIndex` pages


notebookFromChild :: AWidget.WidgetClass child =>
                     child -> IO LNotebook.Notebook
notebookFromChild child = do
  Just parent <- AWidget.widgetGetParent child
  return $ LNotebook.castToNotebook parent


notebookCloseChild :: AWidget.WidgetClass child =>
                      LNotebook.Notebook -> child -> IO ()
notebookCloseChild notebook child = do
  Just n <- getTabPositionByTab notebook child
  LNotebook.notebookRemovePage notebook n
  AWidget.widgetDestroy child

notebookFindAndCloseChild :: AWidget.WidgetClass child => child -> IO ()
notebookFindAndCloseChild child = do
  notebook <- notebookFromChild child
  notebookCloseChild notebook child


notebookSetChildTitle :: AWidget.WidgetClass child =>
                        child -> String -> IO ()
notebookSetChildTitle child title = do
  Just parent <- AWidget.widgetGetParent child
  LNotebook.castToNotebook parent
    `set` [ LNotebook.notebookChildTabLabel child := title ]

