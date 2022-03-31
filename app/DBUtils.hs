{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module DBUtils where

import           Schema

import           Data.Text

import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist            (Entity (..), delete, get, insert)
--import           Database.Persist.Postgresql
import           System.Environment          (getEnv)

import Database.SQLite.Simple as SQ
import Database.SQLite.Simple.FromRow

instance FromRow SiteUrl where
  fromRow = SiteUrl <$> field <*> field <*> field

instance FromRow UrlState where
  fromRow = UrlState <$> field <*> field <*> field

instance FromRow Srv where
    fromRow = Srv <$> field
  

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

instance FromRow BUser where
    fromRow = BUser <$> field <*> field <*> field <*> field

getFirstU :: [BUser] -> Maybe BUser
getFirstU (h:t) = Just h
getFirstU [] = Nothing

getUserById :: Int -> IO (Maybe BUser)
getUserById uid = do
    db_conn <- SQ.open "test2.db"
    sulist <- SQ.queryNamed db_conn "SELECT userId, username, created, chatId FROM BUser WHERE userId=:uid" [":uid" := uid] :: IO [BUser]
    SQ.close db_conn
    
    return (getFirstU sulist)

procNULL :: Maybe Text -> String
procNULL Nothing = ""
procNULL (Just val) =unpack $ val

createUser :: Int -> Maybe Text -> Int -> IO (Int)
createUser userId username chtId = do
  
  db_conn <- SQ.open "test2.db"
  SQ.execute db_conn "INSERT INTO BUser (userId, chatId, username, created) VALUES (?,?,?,datetime('now'))" (userId :: Int, (show chtId) :: String, (procNULL(username) :: String))
  SQ.close db_conn  
  return userId
    
createUser2 :: Int -> String -> IO (Int)
createUser2 id uname= do
    db_conn <- SQ.open "test2.db"
    return id

insertNewUrl :: String -> Int -> IO ()
insertNewUrl urlstr usrId = do
  db_conn <- SQ.open "test2.db"
  SQ.execute db_conn "INSERT INTO UrlList (site_url, state, userId) VALUES (?, 1, ?)"   ( urlstr :: String, usrId :: Int)
  SQ.close db_conn

delSrv :: String -> IO ()
delSrv url = do
    db_conn <- SQ.open "test2.db"
    SQ.execute db_conn "DELETE FROM UrlState WHERE site_url = ? " (Only (url :: String))
    SQ.execute db_conn "DELETE FROM UrlList WHERE site_url = ? " (Only (url :: String))
    --print $ url ++ " - ok"
    SQ.close db_conn



getUrlState :: String -> IO ([UrlState])
getUrlState url= do
  db_conn <- SQ.open "test2.db"
  res <- SQ.queryNamed db_conn "SELECT site_url, url_state,statetime FROM UrlState WHERE site_url = :site_url ORDER BY rowid DESC" [":site_url" := url] :: IO [UrlState]
  SQ.close db_conn
  return res



getUrlList :: Int -> IO ([SiteUrl])
getUrlList uid = do
    db_conn <- SQ.open "test2.db"
    sulist <- SQ.queryNamed db_conn "SELECT site_url, state, userId FROM UrlList WHERE userId=:uid" [":uid" := uid] :: IO [SiteUrl]
    --let lst <- serverList srvlist []
    SQ.close db_conn
    
    return  sulist


getSiteState :: Int -> String
getSiteState 0 = "disable"
getSiteState 1 = "ok"
getSiteState 2 = "not answered"

getUrlListTxt :: [SiteUrl] -> String
getUrlListTxt [] =""
getUrlListTxt ((SiteUrl ur s us):t) = ur ++ "\t" ++(getSiteState s) ++ "\n"++(getUrlListTxt t)
