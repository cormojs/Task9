{-# LANGUAGE RankNTypes #-}
module Types where


import qualified System.IO as IO

import qualified Graphics.UI.Gtk.Abstract.Widget          as AWidget
import qualified Graphics.UI.Gtk.ModelView                as MV
import qualified Graphics.UI.Gtk.Gdk.Pixbuf               as GPixbuf
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SScrolled

import qualified Control.Observer.Synchronous as ObserverSync

type MyImage = (IO.FilePath, GPixbuf.Pixbuf)

data Thumbnails =
  Thumbnails
  { thumbsModel  :: MV.ListStore MyImage
  , thumbsView   :: SScrolled.ScrolledWindow
  , thumbsObserver :: ObserverSync.Sub Int
  }

data Task9View = Task9View {
    appendTab :: AWidget.WidgetClass child
                 => child -> String -> IO Int
  }
