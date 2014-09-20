{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Task9.Nijie where

-- internal
import Graphics.Task9.Types
import Graphics.Task9.MainNotebook
import Graphics.Task9.Utils
import Graphics.Task9.Config

-- Gtk
import Graphics.UI.Gtk (AttrOp((:=), (:=>)), get, set, on, after)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.Abstract.Object    as AObject
import qualified Graphics.UI.Gtk.Abstract.Widget    as AWidget
import qualified Graphics.UI.Gtk.Abstract.Paned     as APaned
import qualified Graphics.UI.Gtk.Buttons.Button     as BButton
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Gdk.Keys           as GKeys
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.Layout.Notebook    as LNotebook
import qualified Graphics.UI.Gtk.Layout.HButtonBox  as LHButtonBox
import qualified Graphics.UI.Gtk.Layout.Table       as LTable
import qualified Graphics.UI.Gtk.Layout.VPaned      as LVPaned
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Multiline.TextView as MTextView
import qualified Graphics.UI.Gtk.Multiline.TextBuffer as MTextBuffer
import qualified Graphics.UI.Gtk.Windows.Window     as WWindow
import qualified Graphics.UI.Gtk.Windows.MessageDialog as WMsgDialog
import qualified Graphics.UI.Gtk.Windows.Dialog     as WDialog

import qualified System.Glib.GError as GError

-- Nijie
import Web.Nijie
import Web.Nijie.Types
import Web.Nijie.Login
import Web.Nijie.JSON

-- bytesring
import           Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8  as BSUTF8

-- text
import qualified Data.Text.IO as TextIO

-- control
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans  as Trans
import qualified Control.Monad.Reader as Reader
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import Control.Applicative ((<$>))
import qualified Control.Concurrent   as Conc
import Control.Exception (SomeException)
import qualified Control.Exception as Exception

-- system
import qualified System.Directory as Dir
import qualified System.Process   as Process


instance Image NijiePage where
  imageName (Link
             (NjeLink { njeTitle = title
                      , njeAuthor = NjeUser { njeUserName = author} })
             _) = BSUTF8.toString title ++ "\n / " ++ BSUTF8.toString author
  imageName (ImageURL _ name _) = name
  imageName (NextLoader i)    = "load next" ++ show i

  imageThumb (Link _ pixbuf)   = pixbuf
  imageThumb (ImageURL _ _ fimg) = imageThumb fimg
  -- imageThumb (NextLoader _)    = imageThumb nextPageImage

  imageFull (Link link _) = do
    savedFilename <- njeSaveTopIllust "./nijie/" link
    GPixbuf.pixbufNewFromFile savedFilename

  imageFull (ImageURL _ _ fimg) = imageFull fimg

  imageThumbAction las@(Link link _) = case njeKind link of
    NjeSingle -> return SinglePageAction
    _ -> return $ AnotherThumbnailsAction $ \store -> do
      urls <- njeIllustUrls link
      let id = Char8.unpack $ njeId link
      Monad.forM_ (zip urls [1..]) $ \(url, i) -> do
        filename <- njeSaveUrl ("./nijie/"++id++"_"++show i) url
        pixbuf <- GPixbuf.pixbufNewFromFile "./res/thumbnail_anime.png"
        let fimg = FileImage filename pixbuf
            name = imageName las
        store `MV.listStoreAppend` ImageURL link name fimg
  imageThumbAction _ = return SinglePageAction
  imageKeyMap = nijieKeyMap



withImage store index action = do
  img <- getLink <$> store `MV.listStoreGetValue` index
  case img of
    Nothing -> return ()
    Just link -> action link

getLink (Link link _)       = Just link
getLink (ImageURL link _ _) = Just link
getLink _                   = Nothing

nijieOpenBrowser (_, store, index) = withImage store index $ \link -> do
  Process.rawSystem "open" [url link]
  return ()
  where url (NjeLink { njeId = id }) = illust ++ Char8.unpack id
        illust = "http://nijie.info/view.php?id="

nijieBookmarkAdd confirm (_, store, index) = withImage store index $ \link -> do
  if confirm
     then withConfirmDialogDo "confirm: fav?" $ njeBookmarkAdd link
     else njeBookmarkAdd link >> putStrLn "illust fav'ed"

nijieNuitaAdd confirm (_, store, index) = withImage store index $ \link -> do
  if confirm
    then withConfirmDialogDo "confirm: nuita?" $ njeNuitaAdd link
    else njeNuitaAdd link >> putStrLn "illust nui'ed"


withConfirmDialogDo :: String -> IO () -> IO ()
withConfirmDialogDo str action = do
  dialog <- WMsgDialog.messageDialogNew
              Nothing [WMsgDialog.DialogModal]
              WMsgDialog.MessageQuestion WMsgDialog.ButtonsOkCancel
              str
  response <- WDialog.dialogRun dialog
  print response
  Monad.when (response == WDialog.ResponseOk) $ action
  AWidget.widgetDestroy dialog

nijieMyFavNew config (self, _, _) = do
  notebook <- notebookFromChild self
  (withNewThumbnailsWithNotebook notebook $ \thumbs ->
    Conc.forkOS $ do
      populateStoreFromNijieThumbs (NjeFav 1) $ thumbsModel thumbs
      populateStoreFromNijieThumbs (NjeFav 2) $ thumbsModel thumbs
      return ()
    ) `Reader.runReaderT` config
  return ()

thumbsNewWithApiOf config (self, store, index) tabHeader tabNamer apiOf =
  Trans.liftIO $ do
    img <- getLink <$> store `MV.listStoreGetValue` index
    case img of
      Nothing -> return ()
      Just link@(NjeLink { njeAuthor = author }) -> do
        author' <- rightAuthor author link
        notebook <- notebookFromChild self
        (withNewThumbnailsWithNotebook notebook $ \thumbs ->
          Conc.forkOS $ do
            size <- populateStoreFromNijieThumbs
              (apiOf $ link { njeAuthor = author'})
              (thumbsModel thumbs)
            notebookSetChildTitle (thumbsView thumbs)
              $ tabHeader ++ BSUTF8.toString (tabNamer author')
                ++ ": " ++ show size
          )`Reader.runReaderT` config
        return ()
  where rightAuthor author link | njeUserId author  /= "-1" = return author
                                | otherwise  = njeRightAuthor link

nijieUserNew config triple =
  thumbsNewWithApiOf config triple "[User] " njeUserName
    (\link@(NjeLink { njeAuthor = author }) ->
      NjeUserIllust author)

nijieUserFavNew config triple =
  thumbsNewWithApiOf config triple "[Favs] " njeUserName
    (\link@(NjeLink { njeAuthor = author }) ->
      NjeUserBookmark author)

nijieUserNuiNew config triple =
  thumbsNewWithApiOf config triple "[Nuis] " njeUserName
    (\link@(NjeLink { njeAuthor = author }) ->
      NjeUserNuita author)

nijieTagSearchPopupNew config triple@(self, store, index) =
  withImage store index $ \link@(NjeLink { njeId = id }) -> do
    doc <- njeDoc $ NjeView id

    (window, box) <- windowButtonBoxNewWithDesc $ njeDescriptionFromDoc doc
    Monad.mapM_ (addTagButton window box)
      $ zip [0..] $ njeTagsFromDoc doc

    AWidget.widgetShowAll window
    where
      addTagButton window tbl (nth, tagName) = do
        button <- BButton.buttonNewWithLabel $ BSUTF8.toString tagName
        let x = nth `mod` 5
            y = nth `div` 5
        LTable.tableAttachDefaults tbl button x (x+1) y (y+1)

        button `on` BButton.buttonActivated $ do
          openSearch tagName
          AWidget.widgetDestroy window
      openSearch tag = do
        thumbsNewWithApiOf config triple "[Search] " (const tag)
          (\_ -> NjeSearch tag NjeSortNui 1)
      windowButtonBoxNewWithDesc desc = do
        window <- WWindow.windowNew
        table <- LTable.tableNew 1 1 False

        table  `set` [ LTable.tableRowSpacing    := 5
                     , LTable.tableColumnSpacing := 5 ]
        descView <- textView desc
        window `set` [ AContainer.containerChild :=> do
                         paned <- LVPaned.vPanedNew
                         APaned.panedPack1 paned descView True  False
                         APaned.panedPack2 paned table    False False
                         return paned
                     ]
        window `on` AWidget.keyPressEvent $ GEventM.tryEvent $ do
          "period" <- GEventM.eventKeyName
          Trans.liftIO $ AWidget.widgetDestroy window

        return (window, table)
      textView str = do
        view <- MTextView.textViewNew
        buf  <- view `get` MTextView.textViewBuffer

        buf  `set` [ MTextBuffer.textBufferText := str ]
        view `set` [ MTextView.textViewWrapMode := MTextView.WrapWord ]
        return view

nijieLogin relogin = do
  isLoggedIn <- Dir.doesFileExist "./session.json"
  Monad.when (relogin || not isLoggedIn) $ do
    NjeLogin email password <- loadJSONFromFile "./login.json"
    njeLoginSave "./session.json" email password


populateStoreFromNijieThumbs :: NjeAPI ->
                                MV.ListStore NijiePage ->
                                IO Int
populateStoreFromNijieThumbs api store = do
  nijieLogin False
  links <- Exception.catch
           (njeLinksFromThumbs api <$> njeDoc api)
           (\e -> do
               print (e :: SomeException)
               return [])
  Monad.forM_ links $ \link@NjeLink { njeId = id, njeThumbUrl = url} -> do
    savedFilename <- njeSaveUrl ("./thumbs/" ++ Char8.unpack id) url
    pixbuf <- GPixbuf.pixbufNewFromFileAtSize savedFilename 200 120
    compositeKindIcon  (njeKind link) pixbuf
    compositeAnimeIcon (njeIsAnime link) pixbuf
    GGeneral.postGUIAsync $ do
      store `MV.listStoreAppend` Link link pixbuf
      return ()
    return ()
  putStrLn $ "added " ++ show (length links) ++ "pics"
  return $ length links
  where
    compositeKindIcon NjeSingle _ = return ()
    compositeKindIcon kind pixbuf = do
      let iconName = if kind == NjeManga
                     then "./res/thumbnail_comic.png"
                     else "./res/thumbnail_dojin.png"
      kindIcon <- GPixbuf.pixbufNewFromFile iconName
      GPixbuf.pixbufComposite kindIcon pixbuf
        1 1  15  15
        1 1 0.5 0.5
        GPixbuf.InterpNearest 255
    compositeAnimeIcon False _ = return ()
    compositeAnimeIcon True pixbuf = do
      animeIcon <- GPixbuf.pixbufNewFromFile "./res/thumbnail_anime.png"
      GPixbuf.pixbufComposite animeIcon pixbuf
        1 15  15  15
        1 15 0.5 0.5
        GPixbuf.InterpNearest 255
