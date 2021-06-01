{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Index where

import Import
import Text.Lucius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

getIndexR :: Handler Html
getIndexR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/index-css.lucius")
        $(whamletFile "templates/index.hamlet")