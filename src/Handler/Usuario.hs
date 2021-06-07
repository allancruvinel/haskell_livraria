{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Text.Lucius

formLogin :: Form (Usuario, Text) 
formLogin = renderDivs $ (,) 
        <$> (Usuario
            <$> areq textField "email " Nothing 
            <*> areq passwordField "senha " Nothing
            ) 
    <*> areq passwordField "Confirm" Nothing  

getUsuarioR :: Handler Html
getUsuarioR  = do
    usuario <- lookupSession "_ID"
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/register-user.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (usuario@(Usuario email senha),conf)  -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div>
                            Email Ja Cadastrado!
                        |] 
                    redirect UsuarioR
                Nothing -> do   
                    if senha == conf then do
                        runDB $ insert usuario
                        setMessage [shamlet|
                            <div>
                                Usuario INSERIDO
                        |] 
                        redirect UsuarioR
                    else do
                        setMessage [shamlet|
                            <div>
                                Senha E CONFIRMACAO INVALIDA
                        |] 
                    redirect UsuarioR   
        _ -> redirect LivrosR


getUsuariosR :: Handler Html
getUsuariosR = do
    usuario <- lookupSession "_ID"
    usuarios <- runDB $ selectList [] [Asc UsuarioEmail]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/livros-css.lucius")
        $(whamletFile "templates/usuarios.hamlet")