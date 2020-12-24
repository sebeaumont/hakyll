--------------------------------------------------------------------------------
-- | Implements a basic static file server for previewing options
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Preview.Server
    ( staticServer
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString                as BS
import           Data.String
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           Network.HTTP.Types.Status      (Status)

--------------------------------------------------------------------------------
import           Hakyll.Core.Logger    (Logger)
import qualified Hakyll.Core.Logger    as Logger
import Hakyll.Preview.Semaphore
import Control.Monad (void, when, forever)
import Control.Exception (tryJust)
import Data.Either (isRight)
import Control.Concurrent (myThreadId, forkIO)
import Data.Time.Clock (getCurrentTime)

staticServer :: Logger               -- ^ Logger
             -> FilePath             -- ^ Directory to serve
             -> String               -- ^ Host to bind on
             -> Int                  -- ^ Port to listen on
             -> Maybe Sema
             -> IO ()                -- ^ Blocks forever
staticServer logger directory host port sem = do
    Logger.header logger $ "Listening on http://" ++ host ++ ":" ++ show port
    Logger.flush logger -- ensure this line is logged before Warp errors
    Warp.runSettings warpSettings
        $ WaiWS.websocketsOr WS.defaultConnectionOptions (application sem) staticApp
  where
    staticApp = Static.staticApp (Static.defaultFileServerSettings directory)
    warpSettings = Warp.setLogger noLog
        $ Warp.setHost (fromString host)
        $ Warp.setPort port Warp.defaultSettings

noLog :: Wai.Request -> Status -> Maybe Integer -> IO ()
noLog _ _ _ = return ()

-- wsocket application
application :: Maybe Sema -> WS.ServerApp
application mSem pending =
  case mSem of
    Nothing -> return ()
    Just sem -> do
      log1 "app"
      wsloop sem pending
      
wsloop :: Sema  -> WS.ServerApp
wsloop sem pend = do
  log1 "accept"
  conn <- WS.acceptRequest pend
  log1 $ "newc" 
  -- this to defaeat warp grim reaper
  -- WS.forkPingThread conn 20
  WS.withPingThread conn 20 (log1 "ping") $ 
    wsloop1 sem conn
  -- wsloop sem pend

-- per-connection loop
wsloop1 :: Sema -> WS.Connection -> IO ()
wsloop1 sem conn = do
  log1 "sync"
  f <- readSema sem
  when f $ do
    log1 "send"
    WS.sendTextData conn ("ACGT" :: BS.ByteString)
  log1 "read"
  -- we should expect an excpetion here whenthe client disconnects
  -- eg on page reload/close
  res <- tryJust connException (WS.receiveDataMessage conn)
  ok <- tryTakeSema sem
  log1 ("take " ++ show ok)
  when (isRight res) $ wsloop1 sem conn
  log1 "close"
  where
    connException :: WS.ConnectionException -> Maybe Bool
    connException (WS.CloseRequest _code _reason) = Just False
    -- this to reap aged connections
    connException WS.ConnectionClosed = Just False
    connException _ = Nothing

log1 :: String -> IO ()
log1 m = do
  tid <- myThreadId
  tim <- getCurrentTime
  putStrLn ("[" <> show tim <> ":" <> show tid <> "]\t" <> m)
