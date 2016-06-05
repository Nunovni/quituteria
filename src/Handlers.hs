{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Quituteria!
    <p>
    <a href=@{ClienteR}>Cadastro de clientes
    <p>
    <a href=@{ListarR}>Clientes Cadastrados
    <p>

|] 

--trexo que estava no main
instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool


instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

--fim do trexo que estava no main


--alterado pelo pessoa
formCliente :: Form Cliente
formCliente = renderDivs $ Cliente <$>
             areq textField "Nome" Nothing <*>
             areq intField "Telefone" Nothing <*>
             areq textField "Endereco" Nothing <*>
             areq textField "Cidade" Nothing
             
{-formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing

formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing


dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades
-}
getHelloR :: Handler Html
getHelloR = defaultLayout [whamlet|
     <h1> _{MsgHello}
|]

-- FUNCAO PARA GERAR FORMULARIOS DE UMA MANEIRA GENERICA
widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

getClienteR :: Handler Html
getClienteR = do
             (widget, enctype) <- generateFormPost formCliente
             defaultLayout $ widgetForm ClienteR enctype widget "Clientes"

getListaCliR :: ClienteId -> Handler Html
getListaCliR cid = do
             cliente <- runDB $ get404 cid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{clienteNome cliente}
                 <p> Telefone: #{clienteTelefone cliente}
                 <p> Endereço: #{clienteEndereco cliente}
                 <p> Cidade: #{clienteCidade cliente}
             |]


--Tela de consulta de clientes cadastrados
getListarR :: Handler Html
getListarR = do
             listaC <- runDB $ selectList [] [Asc ClienteNome]
             defaultLayout $ [whamlet|
                 <h1> Clientes cadastrados:
                 $forall Entity cid cliente <- listaC
                     <a href=@{ListaCliR cid}> #{clienteNome cliente} 
                     <form method=post action=@{ListaCliR cid}> 
                         <input type="submit" value="Deletar"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]



postClienteR :: Handler Html
postClienteR = do
                ((result, _), _) <- runFormPost formCliente
                case result of
                    FormSuccess cliente -> do
                       runDB $ insert cliente 
                       defaultLayout [whamlet| 
                           <h1> #{clienteNome cliente} Inseridx com sucesso. 
                           <input name="" type="button" onClick="@{ClienteR}" value="Voltar">
                       |]
                    _ -> redirect ClienteR

postListaCliR :: ClienteId -> Handler Html
postListaCliR cid = do
     runDB $ delete cid
     redirect ListarR
     
getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing



getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR
               

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{LoginR}>
                     ^{widget}
                     <input type="submit" value="Login">
           |]

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)


getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]
     
getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]