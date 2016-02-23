import Transaction (Account, Transaction)
import CSV (HeaderP, csv, Doc)
import Text.ParserCombinators.Parsec
import System.IO
import Graphics.UI.Gtk
import Control.Exception (bracket, handle, SomeException)
import qualified System.IO.Strict as Strict
import Control.Monad.Trans (liftIO)
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Data.Functor

import Transaction (parseTransactions, selectScheme)
import Graphics.UI.Gtk.ModelView as New

type AppState = ([Transaction], [Account])
data AppError = FileNotFound FilePath
              | UnknownFormat
              | ParseError ParseError

data FunctorIO m a = IO (m a)

instance (Functor m) => Functor (FunctorIO m) where
  fmap f (IO x) = IO (fmap f x)

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

-- Should go into Utils with return type IO (Maybe String)
loadFile :: FilePath -> IO (Either AppError String)
loadFile path = handle handler $ do
  bracket (openFile path ReadMode) hClose (\h -> Right <$> Strict.hGetContents h)
  where handler :: SomeException -> IO (Either AppError String)
        handler _ = return $ Left (FileNotFound path)


parseTransactions :: HeaderP -> String -> Either AppError [Transaction]
parseTransactions = undefined
{--
selectScheme :: FilePath -> Maybe Scheme
parseCSV :: HeaderP -> String -> Either AppError Doc

--}

loadTransactions :: HeaderP -> FilePath -> IO (Either AppError [Transaction])
loadTransactions headerP path = bind (parseTransactions scheme headerP) <$> (loadFile path)
  where bind f = \x -> x >>= f
        scheme = selectScheme path
        {--
  do
    contents <- result
    parseCSV contents
  where parseCSV = adaptError ParseError . parse (csv headerP) path
        bind f = \x -> x >>= f
--}

readCSV :: HeaderP -> FilePath -> IO (Either AppError Doc)
readCSV headerP path = handle handler $ do
  bracket (openFile path ReadMode) hClose $
    \h -> wrapError <$> parseCSV <$> Strict.hGetContents h
  where handler :: SomeException -> IO (Either AppError Doc)
        handler _ = return $ Left (FileNotFound path)
        parseCSV = parse (csv headerP) path
        wrapError (Left e) = Left $ ParseError e
        wrapError (Right doc) = Right doc

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
  store <- storeImpl
  treeViewSetModel (tvAccounts gui) store
  renderer <- New.cellRendererTextNew
  column <- New.treeViewColumnNew
  New.treeViewColumnPackStart column renderer True
  New.cellLayoutSetAttributes column renderer store $ \row -> [New.cellText := row]
  New.treeViewColumnSetTitle column "name"
  _ <- New.treeViewAppendColumn (tvAccounts gui) column
  return ()

redraw :: GUI -> AppState -> IO ()
redraw gui state = undefined

main :: IO ()
main = do
  _ <- initGUI
  gui <- loadGUI "../res/layout.glade"
  connectGUI gui
  widgetShowAll $ winMain gui
  mainGUI
