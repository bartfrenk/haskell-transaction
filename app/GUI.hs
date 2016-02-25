import Transaction

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad.Trans (liftIO)

type GUIState = ([Transaction], [Account])


{--
adaptError :: (e -> e') -> Either e a -> Either e' a
adaptError f (Left e) = Left $ f e
adaptError (Right success) = Right success
--}

data GUI = GUI {
  winMain :: Window,
  miOpen :: ImageMenuItem,
  miQuit :: ImageMenuItem,
  miAbout :: ImageMenuItem,
  tvAccounts :: TreeView
  }


storeImpl :: IO (ListStore String)
storeImpl = New.listStoreNew ["asdsad"]

-- |Returns the GUI object represented by the glade file at specified path
loadGUI :: FilePath -> IO GUI
loadGUI gladePath = do
  builder <- builderNew
  builderAddFromFile builder gladePath
  winMain_ <- builderGetObject builder castToWindow "main"
  miOpen_ <- builderGetObject builder castToImageMenuItem "menu_item_open"
  miQuit_ <- builderGetObject builder castToImageMenuItem "menu_item_quit"
  miAbout_ <- builderGetObject builder castToImageMenuItem "menu_item_about"
  tvAccounts_ <- builderGetObject builder castToTreeView "accounts_view"
  return $ GUI {winMain = winMain_,
                miOpen = miOpen_,
                miQuit = miQuit_,
                miAbout = miAbout_,
                tvAccounts = tvAccounts_}

-- |Connects handlers for signals.
connectGUI :: GUI -> IO ()
connectGUI gui = do
  _ <- (winMain gui) `on` deleteEvent $ liftIO mainQuit >> return False
  _ <- (miQuit gui) `on` menuItemActivated $ liftIO mainQuit
--  _ <- (miOpen gui) `on` menuItemActivated $ fileChooserDialogNew Nothing Nothing FileChooserActionOpen []
  store <- storeImpl
  treeViewSetModel (tvAccounts gui) store
  renderer <- New.cellRendererTextNew
  column <- New.treeViewColumnNew
  New.treeViewColumnPackStart column renderer True
  New.cellLayoutSetAttributes column renderer store $ \row -> [New.cellText := row]
  New.treeViewColumnSetTitle column "name"
  _ <- New.treeViewAppendColumn (tvAccounts gui) column
  return ()

redraw :: GUI -> GUIState -> IO ()
redraw gui state = undefined

main :: IO ()
main = do
  _ <- initGUI
  gui <- loadGUI "res/layout.glade"
  connectGUI gui
  widgetShowAll $ winMain gui
  mainGUI
