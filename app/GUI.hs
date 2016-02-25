{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Transaction

import Utils (loadTransactionFiles, gather)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy (toList, keys)

type GUIState = ([Transaction], [Account])

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
  openDialog :: OpenDialog
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
          loadOpenDialog builder

createBuilder :: FilePath -> IO Builder
createBuilder gladePath = do
  builder <- builderNew
  builderAddFromFile builder gladePath
  return builder

-- |Connects handlers for signals.
connectMainWindow :: GUI -> IO ()
connectMainWindow (GUI mainWin openDlg) = do
  appWindow mainWin `on` deleteEvent $ liftIO mainQuit >> return False
  quitMenuItem mainWin `on` menuItemActivated $ liftIO mainQuit
  openMenuItem mainWin `on` menuItemActivated $ showOpenDialog openDlg
  return ()

connectOpenDialog :: GUI -> IO ()
connectOpenDialog gui = do
  cancelBtn openDlg `on` buttonActivated $ (widgetHide dlg)
  openBtn openDlg `on` buttonActivated $ do
    state <- fileChooserGetFilenames dlg >>= openFiles
    redraw gui state
    widgetHide dlg
  return ()
  where openDlg = openDialog gui
        dlg = chooserDlg openDlg

openFiles :: [FilePath] -> IO GUIState
openFiles paths = do
  ts <- loadTransactionFiles paths
  let accounts = gather trDest trAmount ts
  mapM_ putStrLn (keys accounts)
  return (ts, keys accounts)

connectGUI :: GUI -> IO ()
connectGUI gui = do
  connectMainWindow gui
  connectOpenDialog gui

redraw :: GUI -> GUIState -> IO ()
redraw gui state = do
  store <- New.listStoreNew (snd state)
  treeViewSetModel pane store
  renderer <- New.cellRendererTextNew
  column <- New.treeViewColumnNew
  New.treeViewColumnPackStart column renderer True
  New.cellLayoutSetAttributes column renderer store $ \row -> [New.cellText := row]
  New.treeViewColumnSetTitle column "Account"
  New.treeViewAppendColumn pane column
  return ()
  where pane = accountsPane (mainWindow gui)


main :: IO ()
main = do
  initGUI
  gui <- loadGUI "res/layout.glade"
  connectGUI gui
  widgetShowAll $ appWindow (mainWindow gui)
  mainGUI
