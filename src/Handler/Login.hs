{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Text.Lucius

formLogin :: Form Usuario 
formLogin = renderDivs $ Usuario
    <$> areq textField "email " Nothing 
    <*> areq passwordField "senha " Nothing

getAutR :: Handler Html
getAutR  = do
    usuario <- lookupSession "_ID"
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/login.hamlet")

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario "root@root.com" "root") -> do
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (Usuario email senha) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do
                    setMessage [shamlet|
                        Email não cadastrado!
                    |] 
                    redirect AutR 
                Just (Entity _ usuario) -> do
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (usuarioEmail usuario)
                        redirect IndexR
                    else do
                        setMessage [shamlet|
                            Senha nao confere!
                        |] 
                        redirect IndexR
        _ -> redirect IndexR

postSairR :: Handler Html 
postSairR = do
    deleteSession "_ID"
    redirect IndexR

getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
        BEM VINDO, ADMINISTRADOR
    |]

