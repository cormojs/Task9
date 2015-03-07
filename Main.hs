{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- Internal
import Graphics.Task9.Types
import Graphics.Task9.Thumbnails
import Graphics.Task9.MainNotebook
import Graphics.Task9.PictView
import Graphics.Task9.FileView.FileImageStore
import Graphics.Task9.Nijie
import Graphics.Task9.Utils
import Graphics.Task9.Config

import Web.Nijie.Types (NjeAPI(..), NjeRankType(..), NjeUser(..), NjeSort(..))

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

-- File operation
import System.Environment  (getArgs)

-- Control
import Control.Applicative ((<$>))
import qualified Control.Monad        as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer     as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import qualified Control.Concurrent   as Conc
import Control.Exception (SomeException, PatternMatchFail)
import qualified Control.Exception as Exception



-- bytesring
import           Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8  as BSUTF8

-- network
import qualified Network.Socket as Socket

main :: IO ()
main = Socket.withSocketsDo $ do
  GGeneral.initGUI

  (mode:args) <- getArgs

  (flip Reader.runReaderT) keyConfig $ do
    notebook <- mainNotebookNew
    window <- mainWindowNew notebook
    modeRun notebook mode args
    Trans.liftIO $ WWindow.windowFullscreen window

  GGeneral.mainGUI

modeRun :: LNotebook.Notebook ->
           String -> [String] ->
           Task9IO Conc.ThreadId
modeRun notebook mode args = case mode of
  "file"  -> withNewThumbnailsWithNotebook notebook $ \thumbs ->
    Conc.forkOS
    $ Monad.forM_ args
      $ populateStoreFromFileOrDir $ thumbsModel thumbs
  "nijie" -> withNewThumbnailsWithNotebook notebook $ \thumbs -> do
    let apis = case args of
          ("favs":ps) -> map (NjeFav  . read) ps
          ("like":ps) -> map (NjeLike . read) ps
          ("user":us) -> map (NjeUserIllust .
                              (NjeUser "") . Char8.pack) us
          ("rank":ss) -> map (\s -> NjeRank $ case s of
                                 "now"   -> NjeRankNow
                                 "day"   -> NjeRankDay
                                 "week"  -> NjeRankWeek
                                 "month" -> NjeRankMonth) ss
          ("search":w:ps)
            -> map (NjeSearch (BSUTF8.fromString w) NjeSortNui . read) ps
          _ -> error $ "invalid argument\n" ++ usage
    Conc.forkOS $ do
      addeds <- Monad.forM apis $ \api ->
        (populateStoreFromNijieThumbs api $ thumbsModel thumbs)
        `Exception.catches`
        [ Exception.Handler $ \(e :: PatternMatchFail) -> do
             GGeneral.postGUIAsync
               $ withConfirmDialogDo ("Pattern match fail: "++show e)
                 $ return ()
             return 0
        , Exception.Handler $ \(e :: SomeException) -> do
             GGeneral.postGUIAsync
               $ withConfirmDialogDo ("Other error: "++show e)
                 $ return ()
             print e
             return 0
        ]
      let size = sum addeds
      notebookSetChildTitle (thumbsView thumbs)
        $ head args ++ ": " ++ show size
  _ -> error $ "invalid argument\n" ++ usage


usage :: String
usage = "Usage:\n"
        ++ "\t Task9 file <File or directory>...\n"

keyConfig =
  KeyMapSet { windowKeyMap =
                 [ ("F", WWindow.windowFullscreen)
                 , ("f", WWindow.windowUnfullscreen)
                 , ("q", \w -> do
                       AWidget.widgetDestroy w
                       GGeneral.mainQuit)
                 , ("backslash", \w -> do
                       AWidget.widgetDestroy w
                       GGeneral.mainQuit) ]
            , notebookKeyMap =
                 [ ("L", LNotebook.notebookNextPage)
                 , ("H", LNotebook.notebookPrevPage) ]
            , pictViewKeyMap =
                 [ ("p", (`pictSubChange` subtract 1))
                 , ("n", (`pictSubChange` (+1)))
                 , ("P", (`pictMainChange` subtract 1))
                 , ("N", (`pictMainChange`(+1)))
                 , ("j", pictVScroll (+20.0))
                 , ("k", pictVScroll (subtract 20.0))
                 , ("l", pictHScroll (+20.0))
                 , ("r", pictRotate)
                 , ("R", pictRotateInverse)
                 , ("h", pictHScroll (subtract 20.0))
                 , ("comma", pictToggleResizeState) ]
            , nijieKeyMap =
                 [ ("b", nijieBookmarkAdd True)
                 , ("m", nijieNuitaAdd True)
                 , ("y", nijieMyFavNew keyConfig)
                 , ("u", nijieUserNew  keyConfig)
                 , ("i", nijieUserFavNew keyConfig)
                 , ("o", nijieUserNuiNew keyConfig)
                 , ("O", nijieOpenBrowser)
                 , ("t", nijieTagSearchPopupNew keyConfig)
                 , ("G", const $ nijieLogin True)
                 , ("d", nijieSaveIllustDialog )
                 ]
            , fileViewKeyMap = []
            }


mainWindowNew :: AWidget.WidgetClass w => w
              -> Task9IO WWindow.Window
mainWindowNew widget = do
  keymap <- windowKeyMap <$> Reader.ask
  Trans.liftIO $ do
    window <- WWindow.windowNew
    window `set` [ WWindow.windowDefaultWidth  := 850
                 , WWindow.windowDefaultHeight := 600
                 , AContainer.containerChild   := widget ]
    window `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
      modifiers <- GEventM.eventModifier
      keyname   <- GEventM.eventKeyName
      Trans.liftIO $ do
        let Just action = lookup keyname keymap
        action window
    window `on` AWidget.deleteEvent $ Trans.liftIO $ do
      GGeneral.mainQuit
      return False
    AWidget.widgetShowAll window
  
    return window
