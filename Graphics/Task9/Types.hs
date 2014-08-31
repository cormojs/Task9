{-# LANGUAGE Rank2Types #-}
module Graphics.Task9.Types where


import qualified System.IO as IO

-- gtk
import qualified Graphics.UI.Gtk.Abstract.Widget          as AWidget
import qualified Graphics.UI.Gtk.ModelView                as MV
import qualified Graphics.UI.Gtk.Gdk.Pixbuf               as GPixbuf
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled
import qualified Graphics.UI.Gtk.Display.Image      as DImage
import qualified Graphics.UI.Gtk.Gdk.EventM         as GEventM
import qualified Graphics.UI.Gtk.Windows.Window   as WWindow
import qualified Graphics.UI.Gtk.Layout.Notebook  as LNotebook

import qualified System.Glib.GError as GError

-- observer
import qualified Control.Observer.Synchronous as ObserverSync

-- system
import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import System.FilePath     ((</>))

-- text
import Data.Text (Text)

-- Nijie
import Web.Nijie
import Web.Nijie.Types
import Web.Nijie.Login

-- Control
import qualified Control.Observer as Observer
import qualified Control.Observer.Synchronous as ObserverSync
import Control.Monad.Reader (ReaderT)


-- GUI
data Pict = Pict { pictWindow      :: SScrolled.ScrolledWindow
                 , pictMainState   :: ObserverSync.Sub Int
                 , pictSubState    :: ObserverSync.Sub Int
                 , pictResizeState :: ObserverSync.Sub ResizeState
                 , pictRotateState :: ObserverSync.Sub GPixbuf.PixbufRotation
                 }

-- Config passing
type Task9IO a = ReaderT KeyMapSet IO a

-- Nijie page
data NijiePage = Link NjeLink GPixbuf.Pixbuf
               | ImageURL NjeLink String FileImage
               | NextLoader Int


-- KeyMap
data KeyMapSet =
  KeyMapSet
  { windowKeyMap   :: [ KeyMap WWindow.Window ]
  , notebookKeyMap :: [ KeyMap LNotebook.Notebook ]
  , pictViewKeyMap :: [ KeyMap Pict ]
  , nijieKeyMap    :: [ KeyMap (AWidget.Widget,
                                MV.ListStore NijiePage,
                                Int) ]
  , fileViewKeyMap :: [ KeyMap (AWidget.Widget,
                                MV.ListStore FileImage,
                                Int) ]
  }

type KeyMap a = (Text, a -> IO ())


-- Image Resizer
data ResizeState = NoResize | ResizeInside | ResizeOutside
                 deriving (Enum, Show, Eq, Bounded)


data Thumbnails image =
  Thumbnails { thumbsModel  :: MV.ListStore image
             , thumbsView   :: SScrolled.ScrolledWindow
             , thumbsObserver :: ObserverSync.Sub Int
             }

data ThumbnailClickAction a =
    SinglePageAction 
  | AnotherThumbnailsAction (MV.ListStore a -> IO ())
  | AddThumbnailsAction (MV.ListStore a -> IO ())

-- Image for files
data FileImage = FileImage { imageFilename  :: String
                           , imageThumbnail :: GPixbuf.Pixbuf
                           } deriving Eq

class Image a where
  imageName        :: a -> String
  imageThumb       :: a -> GPixbuf.Pixbuf
  imageFull        :: a -> IO GPixbuf.Pixbuf
  imageThumbAction :: a -> IO (ThumbnailClickAction a)
  imageKeyMap      :: KeyMapSet -> [KeyMap (AWidget.Widget,
                                            MV.ListStore a,
                                            Int)]
  -- imageKeyPressEvent :: AWidget.WidgetClass self
  --                       => self
  --                       -> MV.ListStore a
  --                       -> Int
  --                       -> GEventM.EventM GEventM.EKey Bool


instance Image FileImage where
  imageName  = FilePath.takeFileName . imageFilename
  imageThumb = imageThumbnail
  imageFull (FileImage { imageFilename = filename }) =
    GPixbuf.pixbufNewFromFile filename
  imageThumbAction _ = return SinglePageAction
  imageKeyMap = fileViewKeyMap
    
imageFullNewFromStore :: Image img =>
                         MV.ListStore img -> Int -> IO GPixbuf.Pixbuf
imageFullNewFromStore s i = do
  image <- MV.listStoreGetValue s i
  imageFull image

imageStoreNew :: Image image => IO (MV.ListStore image)
imageStoreNew = do
  store <- MV.listStoreNew []
  MV.customStoreSetColumn store (MV.makeColumnIdString 0) imageName
  MV.customStoreSetColumn store (MV.makeColumnIdPixbuf 1) imageThumb
  return store
