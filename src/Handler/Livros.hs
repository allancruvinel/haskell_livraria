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
    livros <- runDB $ selectList [] [Asc LivrosNome]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/livros-css.lucius")
        $(whamletFile "templates/livros.hamlet")

getPerfilR :: LivrosId -> Handler Html 
getPerfilR cid = do
    livros <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            Pagina do livro #{livrosNome livros}
        <h2>
            Autor #{livrosAutor livros}   
        <h2>
            Categoria #{livrosCategoria livros}               
    |]

postApagarLivroR :: LivrosId -> Handler Html 
postApagarLivroR cid = do
    runDB $ delete cid
    redirect LivrosR