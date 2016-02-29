{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Transaction

import Utils (loadTransactionFiles, gather)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy (keys)
import Control.Monad.Reader
import Control.Monad.State

data AppState = AppState {
  stTransactions :: [Transaction],
  stAccounts :: [Account]
  }

data AppConfig = AppConfig {
  cfgGladePath :: FilePath
  }

newtype AppMonad a = AppMonad {
  getApp :: ReaderT AppConfig (StateT AppState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO,
              MonadReader AppConfig, MonadState AppState)

data MainWindow = MainWindow {
  appWindow :: Window,
  openMenuItem :: ImageMenuItem,
  quitMenuItem :: ImageMenuItem,
  aboutMenuItem :: ImageMenuItem,
  accountsPane :: TreeView
  }

data OpenDialog = OpenDialog {
  chooserDlg :: FileChooserDialog,
  openBtn :: Button,
  cancelBtn :: Button
  }

data GUI = GUI {
  mainWindow :: MainWindow,
  openDialog :: OpenDialog,
  accountStore :: ListStore Account
  }

showOpenDialog :: OpenDialog -> IO ()
showOpenDialog dlg = do
  windowPresent (chooserDlg dlg)
  return ()

-- |Returns the GUI object represented by the glade file at specified path
loadMainWindow :: Builder -> IO MainWindow
loadMainWindow builder = MainWindow <$>
  builderGetObject builder castToWindow "main_window" <*>
  builderGetObject builder castToImageMenuItem "menu_item_open" <*>
  builderGetObject builder castToImageMenuItem "menu_item_quit" <*>
  builderGetObject builder castToImageMenuItem "menu_item_about" <*>
  builderGetObject builder castToTreeView "accounts_view"

loadOpenDialog :: Builder -> IO OpenDialog
loadOpenDialog builder = OpenDialog <$>
  builderGetObject builder castToFileChooserDialog "file_chooser_dlg" <*>
  builderGetObject builder castToButton "open_button" <*>
  builderGetObject builder castToButton "cancel_button"

loadGUI :: FilePath -> IO GUI
loadGUI gladePath = do
  builder <- createBuilder gladePath
  GUI <$> loadMainWindow builder <*>
          loadOpenDialog builder <*>
          New.listStoreNew []

createBuilder :: FilePath -> IO Builder
createBuilder gladePath = do
  builder <- builderNew
  builderAddFromFile builder gladePath
  return builder

-- |Connects handlers for signals.
connectGUI :: GUI -> IO ()
connectGUI gui = do
  connectMainWindow gui
  connectOpenDialog gui
  connectAccountPane gui

connectMainWindow :: GUI -> IO ()
connectMainWindow (GUI mainWin openDlg _) = do
  appWindow mainWin `on` deleteEvent $ liftIO mainQuit >> return False
  quitMenuItem mainWin `on` menuItemActivated $ liftIO mainQuit
  openMenuItem mainWin `on` menuItemActivated $ showOpenDialog openDlg
  return ()

connectOpenDialog :: GUI -> IO ()
connectOpenDialog gui = do
  cancelBtn openDlg `on` buttonActivated $ (widgetHide dlg)
  openBtn openDlg `on` buttonActivated $ do
    st <- fileChooserGetFilenames dlg >>= openFiles
    redraw gui st
    widgetHide dlg
  return ()
  where openDlg = openDialog gui
        dlg = chooserDlg openDlg

connectAccountPane :: GUI -> IO ()
connectAccountPane gui = do
  renderer <- New.cellRendererTextNew
  column <- New.treeViewColumnNew
  New.treeViewColumnPackStart column renderer True
  treeViewSetModel pane store
  New.cellLayoutSetAttributes column renderer store $ \row -> [New.cellText := row]
  New.treeViewColumnSetTitle column "Account"
  New.treeViewAppendColumn pane column
  return ()
  where pane = accountsPane (mainWindow gui)
        store = accountStore gui

openFiles :: [FilePath] -> AppMonad ()
openFiles paths = do
  ts <- liftIO $ loadTransactionFiles paths
  let accounts = gather trDest trAmount ts
  put (AppState ts (keys accounts))
  return ()

initialState :: IO (ListStore Account)
initialState = New.listStoreNew []

redraw :: GUI -> AppState -> IO ()
redraw gui st = do
  listStoreClear store
  forM_ (stAccounts st) (listStoreAppend store)
  where store = accountStore gui

main :: IO ()
main = do
  initGUI
  gui <- loadGUI "res/layout.glade"
  connectGUI gui
  widgetShowAll $ appWindow (mainWindow gui)
  mainGUI
