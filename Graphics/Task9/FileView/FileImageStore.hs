module Graphics.Task9.FileView.FileImageStore where

-- Internal
import Graphics.Task9.Types


-- Gtk
import Graphics.UI.Gtk (AttrOp((:=)), set, on)

import qualified Graphics.UI.Gtk.Abstract.Container as AContainer
import qualified Graphics.UI.Gtk.General.Enums      as GEnums
import qualified Graphics.UI.Gtk.General.General    as GGeneral
import qualified Graphics.UI.Gtk.ModelView          as MV
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Gdk.Pixbuf         as GPixbuf
import qualified Graphics.UI.Gtk.Display.Image      as DImage

-- System
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import System.FilePath     ((</>))

-- Control
import qualified Control.Monad as Monad
import Control.Applicative ((<$>))

-- Data
import Data.List (sort)


populateStoreFromFileOrDir :: MV.ListStore FileImage -> FilePath -> IO ()
populateStoreFromFileOrDir store path = do
  isDir  <- Dir.doesDirectoryExist path
  isFile <- Dir.doesFileExist path
  Monad.when isDir  $ populateStoreFromDir  store path
  Monad.when isFile $ populateStoreFromFile store path

populateStoreFromDir :: MV.ListStore FileImage -> String -> IO ()
populateStoreFromDir store dir = do
  let isPict f = FilePath.takeExtension f `elem` [".jpg", ".jpeg", ".png"]
  files <- Monad.filterM Dir.doesFileExist
           =<< (map (dir</>) <$> Dir.getDirectoryContents dir)
  Monad.mapM_ (populateStoreFromFile store) $ sort $ filter isPict files

populateStoreFromFile store filename = do
  pixbuf <- GPixbuf.pixbufNewFromFileAtSize filename 240 300
  GGeneral.postGUIAsync $ do
    store `MV.listStoreAppend` (FileImage filename pixbuf)
    return ()

