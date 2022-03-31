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
module Main where
    
import Bot                  --(botStartup)
import Schema                      --(doMigration)
import Control.Monad.IO.Class  (liftIO)



import System.IO  
import Control.Monad
import Control.Concurrent.MVar
import Data.Monoid()
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Control.Applicative

import System.Cron.Schedule

  
main :: IO ()
main = do
    {-test "http://bit.icc.ru"-}
    --test "http://irk.ru"
    --pingServer "http://cris.icc.ru" --!!!!
    --pingServer "http://irk.ru" "http://irk.ru"
    --getUrlFromFile "test.txt"
    --checkAll 1
    
    --tids <- execSchedule $ addJob (doTesting) "* * * * *"
    botStartup
    
    --pingServer "http://irk.ru"
    --pingServer "http://bit.icc.ru"
    --print (splitBy ' ' "1  url")
    {-send_crash_mail "http://bit.icc.ru"-}

    {-print $ getResponseHeader "Content-Type" response
    LC.putStrLn "ok"-}

    
    

   
{-main = putStrLn "Hello, Haskell1111!"
http://bit.icc.ru

warp

main = httpLBS "http://news.ycombinator.com"
-}