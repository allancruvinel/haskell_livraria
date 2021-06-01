{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Register where

import Import
import Text.Lucius

formRegister :: Maybe Livros -> Form Livros 
formRegister ml = renderDivs $ Livros
    <$> areq textField "Nome " (fmap livrosNome ml) 
    <*> areq textField "Autor " (fmap livrosAutor ml) 
    <*> areq textField "Categoria " (fmap livrosCategoria ml)  
    

getRegisterR :: Handler Html
getRegisterR = do
    (widget,_) <- generateFormPost (formRegister Nothing)
    usuario <- lookupSession "_ID"
    msg <- getMessage 
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/register.hamlet")

postRegisterR :: Handler Html
postRegisterR = do
    ((result,_),_) <- runFormPost (formRegister Nothing)
    case result of
        FormSuccess livros -> do
            runDB $ insert livros
            setMessage [shamlet|
                <div>
                    LIVRO INSERIDO
            |] 
            redirect LivrosR 
        _ -> redirect LivrosR

getEditarLivR :: LivrosId -> Handler Html
getEditarLivR liv = do
    livro <- runDB $ get404 liv
    usuario <- lookupSession "_ID"
    (widget,_) <- generateFormPost (formRegister (Just livro))
    msg <- getMessage 
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/update.hamlet")

postEditarLivR :: LivrosId -> Handler Html
postEditarLivR liv = do
    _ <- runDB $ get404 liv
    ((result,_),_) <- runFormPost (formRegister Nothing)
    case result of 
        FormSuccess novoLivro -> do
            runDB $ replace liv novoLivro
            redirect LivrosR
        _ -> redirect IndexR