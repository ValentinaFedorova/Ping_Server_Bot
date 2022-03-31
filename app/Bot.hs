{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Bot where

import           DBUtils
import           Schema

import           Control.Applicative              ((<|>))
import           Control.Monad.IO.Class           (liftIO)
import           Data.Maybe
import           Data.Text
import           Data.Time                        (getCurrentTime)
import           System.Environment               (getEnv)
import           Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

import Control.Exception          (try)
import Network.HTTP.Simple
import Network.Socket
--import Network.Mail.SMTP
import Database.SQLite.Simple as SQ
import Database.SQLite.Simple.FromRow

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC



import System.IO  
import Data.Time
import Data.Time.Format

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Integer Int Text
  deriving (Show, Read)

botStartup :: IO ()
botStartup
    -- logging
 = do
  let env_token = "your telegram token here"
  let token = Token . pack $ env_token
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId incexpBotApp) env

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate


badSite :: IO(String)
badSite = do
  db_conn <- SQ.open "test2.db"
  ((SiteUrl site_url state userId) :t) <- SQ.queryNamed db_conn "SELECT * FROM UrlList WHERE state = 2" [] :: IO [SiteUrl]
  SQ.close db_conn
  return site_url
  badSite :: IO(String)

badSiteUserId :: IO(Int)
badSiteUserId = do
  db_conn <- SQ.open "test2.db"
  ((SiteUrl site_url state userId) :t) <- SQ.queryNamed db_conn "SELECT * FROM UrlList WHERE state = 2" [] :: IO [SiteUrl]
  SQ.close db_conn
  return userId


bchatId :: IO( String)
bchatId = do
    uid <-liftIO $ badSiteUserId 
    Just  (BUser userId username created chartId) <- getUserById uid
    return (unpack chartId)

csendMessage :: BUser -> String -> String -> IO()
csendMessage (BUser userId username created chartId) msg url= do
  print chartId
  print "send error"
  print msg
  let cId=read (unpack chartId)::Integer
  let chtId = ChatId cId
  let chatId=SomeChatId chtId
  let msg2=toReplyMessage (pack msg)
  let r=replyTo chatId msg2
  db_conn <- SQ.open "test2.db"
  --SQ.execute db_conn "INSERT INTO UrlState (site_url,url_state,statetime) VALUES (?,-1,datetime('now'))" (Only (url :: String))
  SQ.execute db_conn "INSERT INTO UrlState (site_url,url_state,statetime) VALUES (?,0,datetime('now'))" (Only (url :: String))
  SQ.execute db_conn "UPDATE UrlList SET state=2 WHERE site_url=?" (Only (url :: String))
  SQ.close db_conn


saveErrorState2 :: String -> Int -> HttpException -> IO ()
saveErrorState2 url usrId e = do
    Just  u <- getUserById usrId
    csendMessage u (url++" is not working") url
    --let r=replyTo usrId "ok"
    

saveOkState :: String -> IO ()
saveOkState url = do
    db_conn <- SQ.open "test2.db"
    SQ.execute db_conn "INSERT INTO UrlState (site_url,url_state,statetime) VALUES (?,1,datetime('now'))" (Only (url :: String))
    SQ.execute db_conn "UPDATE UrlList SET state=1 WHERE  site_url=?" (Only (url :: String))
    print $ url ++ " - ok"
    SQ.close db_conn
pingServer2 :: SiteUrl ->IO()
pingServer2 (SiteUrl site_url state userId) = do
    print site_url
    req <- parseRequest site_url
    response2 <- try $ httpLBS req 
    case response2 of
      Left e -> saveErrorState2 site_url userId e
      Right result -> saveOkState site_url
        
doTesting :: IO ()
doTesting = do
    db_conn <- SQ.open "test2.db"
    srvlist <- SQ.queryNamed db_conn "SELECT * FROM UrlList WHERE state <>0" [] :: IO [SiteUrl]
    SQ.close db_conn
    mapM_ (pingServer2) $ srvlist

sendChatAction :: ChatModel -> Eff Action ChatModel
sendChatAction  model =
  model <# do
    pure NoAction



testing :: BotJob ChatModel Action
testing = BotJob {botJobSchedule ="* * * * *", botJobTask = handleAction2 }

incexpBotApp :: BotApp ChatModel Action
incexpBotApp =
  BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = [testing]}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate model update =
  let msg = fromJust $ updateMessage update
      usr = fromJust $ messageFrom msg
      Telegram.UserId usrId = Telegram.userId usr
      Telegram.MessageId msgId = Telegram.messageMessageId msg

      cht = messageChat msg
      Telegram.ChatId chtId = Telegram.chatId cht
      cId = fromIntegral chtId ::Int

      usrIdInt = fromIntegral usrId :: Int
      msgIdInt = fromIntegral msgId :: Int
      usrName = Telegram.userUsername usr
      parser = RecordMsg usrIdInt usrName chtId msgIdInt <$> plainText 
   in parseUpdate parser update


splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = Prelude.foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
          | currentChar == delimiterChar = "":allStrings -- start a new partial string at the head of the list of all strings
          | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string

getFirstC :: [String] -> String
getFirstC (h:t) = h

getSecondC :: [String] -> String
getSecondC (h:t) = getFirstC t

getCommand :: Text -> IO (String)
getCommand msg = do
  let x = getFirstC (splitBy ' ' (unpack msg))
  return x

getUrl :: Text -> IO (String)
getUrl msg = do
  let x = getSecondC (splitBy ' ' (unpack msg))
  return x
   


analyseLog :: [UrlState] -> Int -> Int -> String -> String
analyseLog ((UrlState url 1 wtime):t) 1 cnt site = site++", all is ok. Pinging started at "++show wtime
analyseLog ((UrlState url 0 _):t) 1 cnt site = analyseLog t 0 (cnt+1) site
analyseLog ((UrlState url 0 _):t) 0 cnt site = analyseLog t 0 (cnt+1) site
analyseLog [] 1 cnt site = site ++ " - missing. You can add him. Comand: add "++site
analyseLog [] 0 cnt site = site ++ " - never working. The call count is "++show cnt
analyseLog ((UrlState url 1 time1):t) 0 cnt site=  "server " ++ url ++ " is not working. The last working time is "++ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" time1
--"site has not been working "++ 



saveErrorState :: String -> HttpException -> IO ()
saveErrorState url e = do
    db_conn <- SQ.open "test2.db"
    --SQ.execute db_conn "INSERT INTO UrlState (site_url,url_state,statetime) VALUES (?,-1,datetime('now'))" (Only (url :: String))
    SQ.execute db_conn "INSERT INTO UrlState (site_url,url_state,statetime) VALUES (?,0,datetime('now'))" (Only (url :: String))
    res <- SQ.queryNamed db_conn "SELECT site_url, url_state,statetime FROM UrlState WHERE site_url = :site_url ORDER BY rowid DESC" [":site_url" := url] :: IO [UrlState]
    print (analyseLog res 1 0 url)
    SQ.close db_conn


pingServer :: String -> IO()
pingServer url = do 
    req <- parseRequest url
    response2 <- try $ httpLBS req 
    case response2 of
        Left e -> saveErrorState url e
        Right result -> saveOkState url


serverList :: [SiteUrl] -> [String] -> [String]
serverList [] l = l
serverList ((SiteUrl site_url state userId ):t) l = serverList t (site_url:l)


f :: [String] -> [Int]
f = Prelude.map read    

printList :: [String] -> String -> String

printList (h:t) "" = printList t h
printList (h:t) headstr = headstr


addFunc :: String -> IO()
addFunc mystr = do
  putStrLn (mystr ++ " done")

getUrlFromFile :: String -> IO()

getUrlFromFile filename = do
    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    --let singlewords = words contents
    --    list = f singlewords
    let content_lines = Prelude.lines contents
    --mapM_ (addFunc) content_lines 
    mapM_ (pingServer) content_lines
    --putStrLn  (printList content_lines "")
    hClose handle
    --return content_lines
    {-list = f singlewords
            print list
            hClose handle   
            -}


-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = Data.Text.unlines
 [ "Hi. There's a command list:"
 ,"1) add url"
 ,"2) remove url"
 ,"3) show - print all urls"
 ,"4) ping url - to ping url"
 ,"5) analyze url"
 ,"6) pingAll - to ping all urls"
 ]   

handleAction :: Action -> ChatModel -> Eff Action ChatModel
handleAction action model =
  case action of
    NoAction -> pure model
    RecordMsg usrId usrname chtId msgId txt ->
      model <# do
        let cId = fromIntegral chtId ::Int
        maybeUser <- liftIO $ getUserById usrId
        case maybeUser of
          Just user -> do
            now <- liftIO getCurrentTime
            comand <- liftIO $ getCommand txt --(unpack txt)
            liftIO $ print comand
            url <- liftIO $ getUrl txt
            liftIO $ print cId
            let chtId2 = ChatId chtId
            let chatId=SomeChatId chtId2
            case comand of
              "add" -> do
                liftIO $ insertNewUrl url usrId
                replyString "url added"
              "remove" -> do
                liftIO $ delSrv url
                replyString "url removed"
              "show" -> do
                sitelist<-liftIO $ getUrlList usrId
                replyString (getUrlListTxt sitelist)
              "help" -> do
                replyString (unpack  startMessage)
              "analyze" -> do
                stateList <- liftIO $ getUrlState url
                replyString (analyseLog stateList 1 0 url)
              "ping" -> do
                liftIO $ print ("pinging"++url)
                liftIO $ pingServer url
                sitelist<-liftIO $ getUrlList usrId
                replyString (getUrlListTxt sitelist)
              "pingAll" -> do
                  liftIO $ doTesting
                  sitelist<-liftIO $ getUrlList usrId
                  replyString (getUrlListTxt sitelist)
              _ -> do
                replyString (unpack  startMessage)
              
          Nothing -> do
            userKy <- liftIO $ (createUser usrId usrname cId)
            replyString "Hi. There's a command list: \n 1) add url \n 2) remove url \n 3) show - print all urls \n 4) ping - to ping all added url"
        --_ <- liftIO $ insertMsg $ Schema.Message msgId (Schema.UserKey usrId) txt now
         
        
        replyString "Done"
        pure NoAction
        
replyString :: String -> BotM ()
replyString = reply . toReplyMessage . pack
