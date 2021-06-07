{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Locacao where

import Import
import Database.Persist.Postgresql
import Text.Lucius

formLocacao :: UsuarioId -> Form Locacao 
formLocacao cid = renderDivs $ Locacao
    <$> pure cid
    <*> areq (selectField livCB) "Livros: "Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq intField "Quantidade: " Nothing
    

-- [(
-- [(Lapis,Key 1),(Borracha,Key 2),...]
livCB :: Handler (OptionList (Key Livros))
livCB = do
    livros <- runDB $ selectList [] [Asc LivrosNome]
    optionsPairs $
        map (\r -> (livrosNome $ entityVal r, entityKey r)) livros

userCB :: Handler (OptionList (Key Usuario))
userCB = do
    usuarios <- runDB $ selectList [] [Asc UsuarioEmail]
    optionsPairs $
        map (\r -> (usuarioEmail $ entityVal r, entityKey r)) usuarios

getLocacaoR :: UsuarioId -> Handler Html
getLocacaoR cid = do
    usuario <- lookupSession "_ID"
    (widget,_) <- generateFormPost (formLocacao cid)
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/livros-css.lucius")
        $(whamletFile "templates/locacao.hamlet")

postLocacaoR :: UsuarioId -> Handler Html
postLocacaoR cid = do
    ((result,_),_) <- runFormPost (formLocacao cid)
    case result of
        FormSuccess locacao -> do
            runDB $ insert locacao
            setMessage [shamlet|
                <div>
                    Locacao Inserida com sucesso
            |]
            redirect (AlugadosR cid)
        _ -> redirect IndexR

getAlugadosR :: UsuarioId -> Handler Html
getAlugadosR cid = do 
    usuario <- lookupSession "_ID"
    let sql = "SELECT ??,??,?? FROM livros \
          \ INNER JOIN locacao ON locacao.livid = livros.id \
          \ INNER JOIN usuario ON locacao.userid = usuario.id \
          \ WHERE usuario.id = ?"
    usuarioloca <- runDB $ get404 cid
    tudo <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Livros,Entity Locacao,Entity Usuario)]
    defaultLayout $ do 
        toWidgetHead $(luciusFile "templates/livros-css.lucius")
        $(whamletFile "templates/alugados.hamlet")

postApagarLocacaoR :: LocacaoId -> Handler Html 
postApagarLocacaoR cid = do
    runDB $ delete cid
    setMessage [shamlet|
                <div>
                    Devolvido com sucesso!
            |]
    redirect UsuariosR

--, #{mult (LivrosAutor produto) (fromIntegral (vendaQt venda))} no dia #{show $ vendaDia venda}