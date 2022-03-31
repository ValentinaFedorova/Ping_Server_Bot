{-# LANGUAGE ExistentialQuantification  #-}
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


module Schema where

import           Control.Monad.IO.Class
import           Data.Text
import           Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UrlState
    url String
    state Int
    statetime UTCTime default=CURRENT_TIME
    deriving Show
BUser
    userId Int
    username Text Maybe
    created UTCTime default=CURRENT_TIME
    chartId Text
    deriving Show
SiteUrl
    site_url String
    state Int
    userId Int
    deriving Show
SiteCall
    site_url String
    state Int
    chartId Text
    deriving Show
Srv
    url String
    deriving Show
|]


