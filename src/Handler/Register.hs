{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Register where

import Import
import Text.Lucius

formRegister :: Form Livros 
formRegister = renderDivs $ Livros
    <$> areq textField "Nome " Nothing 
    <*> areq textField "Autor " Nothing 
    <*> areq textField "Categoria " Nothing 
    

getRegisterR :: Handler Html
getRegisterR = do
    (widget,_) <- generateFormPost formRegister
    msg <- getMessage 
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/register-css.lucius")
        $(whamletFile "templates/register.hamlet")

postRegisterR :: Handler Html
postRegisterR = do
    ((result,_),_) <- runFormPost formRegister
    case result of
        FormSuccess livros -> do
            runDB $ insert livros
            setMessage [shamlet|
                <div>
                    LIVRO INSERIDO
            |] 
            redirect LivrosR 
        _ -> redirect LivrosR