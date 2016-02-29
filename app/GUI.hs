{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Transaction

import Utils (dedup)
import Control.Concurrent.STM (readTVarIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Core

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

-- TODO: do not need to bind all
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
connectGUI :: GUI -> AppData -> IO ()
connectGUI gui appData = do
  connectMainWindow gui
  connectOpenDialog gui appData
  connectAccountPane gui

connectMainWindow :: GUI -> IO ()
connectMainWindow (GUI mainWin openDlg _) = do
  appWindow mainWin `on` deleteEvent $ liftIO mainQuit >> return False
  quitMenuItem mainWin `on` menuItemActivated $ liftIO mainQuit
  openMenuItem mainWin `on` menuItemActivated $
    windowPresent (chooserDlg openDlg) >> return ()
  return ()

connectOpenDialog :: GUI -> AppData -> IO ()
connectOpenDialog gui appData = do
  cancelBtn openDlg `on` buttonActivated $ (widgetHide dlg)
  openBtn openDlg `on` buttonActivated $ do
    paths <- fileChooserGetFilenames dlg
    runAppM (appendTransactions paths) appData
    readTVarIO (transactionsT appData) >>= redraw gui
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

-- TODO: remove dependency on the internals of AppData, without
-- allowing 'redraw' to modify the global state.
redraw :: GUI -> [Transaction] -> IO ()
redraw gui ts = do
  listStoreClear store
  let accounts = dedup (trDest <$> ts)
  forM_ accounts (listStoreAppend store)
  where store = accountStore gui

main :: IO ()
main = do
  initGUI
  gui <- loadGUI "res/layout.glade"
  initAppData >>= connectGUI gui
  widgetShowAll $ appWindow (mainWindow gui)
  mainGUI
  return ()
