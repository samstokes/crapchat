{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Network.Wai.Middleware.RequestLogger
import System.Environment (getArgs)
import Web.Scotty.Trans

type Channel = Chan Text

data ChatState = ChatState { chatChannels :: Map Text Channel, chatNextUserId :: Int }

instance Default ChatState where
  def = ChatState Map.empty 0

newtype Chat a = Chat { runChat :: ReaderT (TVar ChatState) IO a }
  deriving (Functor, Monad, MonadIO, MonadReader (TVar ChatState))

chat :: MonadTrans t => Chat a -> t Chat a
chat = lift

main :: IO ()
main = do
  port <- read . head <$> getArgs

  initialChannels <- newTVarIO def

  let runM m = runReaderT (runChat m) initialChannels
      initM = runM
      runActionToIO = runM

  scottyT port initM runActionToIO $ do
    middleware logStdoutDev
    app

app :: ScottyT Text Chat ()
app = do
  get "/:chan" $ do
    chanName <- param "chan"

    contentType <- fmap parseAccept <$> header "Accept"

    case contentType of
      Just "text/html" -> do
        (channel, uid) <- session chanName Nothing
        _ <- liftIO $ forkIO $ do
          threadDelay 1000000 
          writeChan channel $ mconcat ["Chat: Welcome, user ", Text.pack $ show uid]

        html $ chatPage chanName uid
      Just "application/json" -> do
        uid <- param "uid"
        (channel, _) <- session chanName $ Just uid
        msg <- waitForMsg channel
        json msg
      Just other -> raise $ mconcat ["Unknown Accept: ", other]
      Nothing -> raise "Missing Accept!"

  post "/:chan" $ do
    chanName <- param "chan"
    uid <- param "uid"
    msg <- decodeUtf8 <$> body

    (channel, _) <- session chanName $ Just uid

    liftIO $ writeChan channel $ mconcat [Text.pack $ show uid, ": ", msg]

session :: Text -> Maybe Int -> ActionT Text Chat (Channel, Int)
session chanName muid = do
  newChannel <- liftIO newChan

  state <- chat ask

  liftIO $ atomically $ do
    ChatState {chatChannels = channels, chatNextUserId = nextUid} <- readTVar state
    let (uid, newUid) = maybe (nextUid, succ nextUid) (, nextUid) muid
        (channel, newChannels) = case Map.lookup chanName channels of
                                   Just existingChan -> (existingChan, channels)
                                   Nothing -> (newChannel, Map.insert chanName newChannel channels)
    writeTVar state ChatState {chatChannels = newChannels, chatNextUserId = newUid}
    return (channel, uid)

parseAccept :: Text -> Text
parseAccept = Text.takeWhile (/= ',')

waitForMsg :: Channel -> ActionT Text Chat Text
waitForMsg channel = liftIO $ dupChan channel >>= readChan

chatPage :: Text -> Int -> Text
chatPage chanName uid = layout chanName uid $ mconcat
  [ "<p>You are user ", Text.pack $ show uid, "</p>"
  , "<section id='chat'>"
  , "</section>"
  , chatForm chanName
  ]

layout :: Text -> Int -> Text -> Text
layout chanName uid bdy = mconcat [hdr, bdy, ftr] where
  hdr = mconcat
    [ "<script src='//code.jquery.com/jquery-2.1.1.min.js'></script>"
    , "<script>\n"
    , "  function getMsg() {\n"
    , "    $.getJSON('/", chanName, "?uid=", Text.pack $ show uid, "').then(function (data) {\n"
    , "      $('#chat').append('<p>' + data + '</p>');\n"
    , "      getMsg();\n"
    , "    });\n"
    , "  }\n"
    , "  $(function () {\n"
    , "    getMsg();\n"
    , "    $('form').submit(function (event) {\n"
    , "      event.preventDefault();\n"
    , "      var input = $('input');\n"
    , "      $.post('/", chanName, "?uid=", Text.pack $ show uid, "', input.val());\n"
    , "      input.val('');\n"
    , "    });\n"
    , "  });\n"
    , "</script>"
    ]
  ftr = mempty

chatForm :: Text -> Text
chatForm chanName = mconcat ["<form method='POST' action='/", chanName, "'><input name='msg'/></form>"]
