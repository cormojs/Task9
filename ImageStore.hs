module ImageStore where

-- Internal 
import Types


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


imageStoreNew :: IO (MV.ListStore MyImage)
imageStoreNew = do
  store <- MV.listStoreNew ([] :: [MyImage])
  MV.customStoreSetColumn store (MV.makeColumnIdString 0)
    (take 20 . FilePath.takeFileName . imageFilename)
  MV.customStoreSetColumn store (MV.makeColumnIdPixbuf 1) imageThumb
  return store


populateStoreFromFileOrDir store path = do
  isDir  <- Dir.doesDirectoryExist path
  isFile <- Dir.doesFileExist path
  Monad.when isDir  $ populateStoreFromDir  store path
  Monad.when isFile $ populateStoreFromFile store path

populateStoreFromDir :: MV.ListStore MyImage -> String -> IO ()
populateStoreFromDir store dir = do
  let isPict f = FilePath.takeExtension f `elem` [".jpg", ".jpeg", ".png"]
  files <- Monad.filterM Dir.doesFileExist
           =<< (map (dir</>) <$> Dir.getDirectoryContents dir)
  Monad.mapM_ (populateStoreFromFile store) $ sort $ filter isPict files

populateStoreFromFile store filename = do
  pixbuf <- GPixbuf.pixbufNewFromFileAtSize filename 240 300
  GGeneral.postGUIAsync $ do
    store `MV.listStoreAppend` (MyImage filename pixbuf Nothing)
    return ()

imageNewFromStore :: MV.ListStore MyImage -> Int -> IO DImage.Image
imageNewFromStore s i = do
  MyImage { imageFilename = filename } <- MV.listStoreGetValue s i
  DImage.imageNewFromFile filename
