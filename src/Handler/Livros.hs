{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Livros where

import Import
import Text.Lucius

getLivrosR :: Handler Html
getLivrosR = do
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/livros-css.lucius")
        $(whamletFile "templates/livros.hamlet")