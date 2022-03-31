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
    botStartup
    
 
