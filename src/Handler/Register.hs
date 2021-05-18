{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Register where

import Import
import Text.Lucius

getRegisterR :: Handler Html
getRegisterR = do
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/register.hamlet")