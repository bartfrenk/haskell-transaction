{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Transaction

import Control.Concurrent.STM (readTVarIO)
import Graphics.UI.Gtk
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.Monoid (getSum)
import Core

-- TODO: clean up, no need to remember widgets once they are connected
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
  accountStore :: ListStore AccountStats
  }

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
          listStoreNew []

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
  treeViewSetModel pane store
  renderer <- cellRendererTextNew
  let append = appendColumn pane renderer store
  append "Account" (\(account, _, _, _) -> account)
  append "Count" (\(_, count, _, _) -> show $ getSum count)
  append "Debet" (\(_, _, debet, _) -> show debet)
  append "Credit" (\(_, _, _, credit) -> show credit)
  where pane = accountsPane (mainWindow gui)
        store = accountStore gui

appendColumn :: TreeView -> CellRendererText -> ListStore AccountStats ->
                String -> (AccountStats -> String) -> IO ()
appendColumn tv rend store title f = do
  column <- treeViewColumnNew
  treeViewColumnPackStart column rend True
  cellLayoutSetAttributes column rend store $
    \summary -> [cellText := f summary]
  treeViewColumnSetTitle column title
  treeViewAppendColumn tv column
  return ()

-- TODO: remove dependency on the internals of AppData, without
-- allowing 'redraw' to modify the global state.
-- TODO: move summary function to Core
redraw :: GUI -> [Transaction] -> IO ()
redraw gui ts = do
  listStoreClear store
  forM_ (allAccountStats ts) (listStoreAppend store)
  where store = accountStore gui

main :: IO ()
main = do
  initGUI
  gui <- loadGUI "res/layout.glade"
  initAppData >>= connectGUI gui
  widgetShowAll $ appWindow (mainWindow gui)
  mainGUI
  return ()
