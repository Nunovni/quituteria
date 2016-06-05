{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Time
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Quituteria!
    <p>
    <a href=@{ClienteR}>Cadastro de Clientes
    <p>
    <a href=@{ProdR}>Cadastro de Produtos
    <p>
    <a href=@{ListarR}>Clientes Cadastrados
    <p>
    <a href=@{ListProdR}>Produtos Cadastrados
    <p>
    <a href=@{PedidoR}>Cadastrar Pedidos
    <p>
    <p>
    <p>
    <p>
    <a href=@{LoginR}>Efetuar Login
    <p>
    <a href=@{LogoutR}>Efetuar Logout
    <p>
    
    
    
    

|] 


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




--Formularios
formCliente :: Form Cliente
formCliente = renderDivs $ Cliente <$>
             areq textField "Nome" Nothing <*>
             areq intField "Telefone" Nothing <*>
             areq textField "Endereco" Nothing <*>
             areq textField "Cidade" Nothing
        

formProduto :: Form Produto
formProduto = renderDivs $ Produto <$>
             areq textField "Nome" Nothing <*>
             areq textField "Descrição" Nothing <*>
             areq doubleField "Valor" Nothing
             
formPedido :: Form Pedido
formPedido = renderDivs $ Pedido <$>
             areq (selectField clint) "Cliente" Nothing <*>
             areq dayField "Entrega" Nothing <*>
             areq doubleField "Total" Nothing

             
clint = do
       entidades <- runDB $ selectList [] [Asc ClienteNome] 
       optionsPairs $ fmap (\ent -> (clienteNome $ entityVal ent, entityKey ent)) entidades
     

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

getProdR :: Handler Html
getProdR = do
             (widget, enctype) <- generateFormPost formProduto
             defaultLayout $ widgetForm ProdR enctype widget "Produto"


getListaCliR :: ClienteId -> Handler Html
getListaCliR cid = do
             cliente <- runDB $ get404 cid 
             defaultLayout [whamlet| 
                 <h1> Dados do cliente: #{clienteNome cliente}
                 <p> Telefone: #{clienteTelefone cliente}
                 <p> Endereço: #{clienteEndereco cliente}
                 <p> Cidade: #{clienteCidade cliente}
             |]

getListaProdR :: ProdutoId -> Handler Html
getListaProdR pid = do
             produto <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                 <h1> Dados do produto #{produtoNome produto}
                 <p> Descrição: #{produtoDescricao produto}
                 <p> Valor: R$#{produtoValor produto}
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


getListProdR :: Handler Html
getListProdR = do
             listaP <- runDB $ selectList [] [Asc ProdutoNome]
             defaultLayout $ [whamlet|
                 <h1> Produtos cadastrados:
                 $forall Entity pid produto <- listaP
                     <a href=@{ListaProdR pid}> #{produtoNome produto} 
                     <form method=post action=@{ListaProdR pid}> 
                         <input type="submit" value="Deletar"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]
{-
getListPedR :: Handler Html
getListPedR = do
             listaPed <- runDB $ selectList [] [Asc PedidoId]
             defaultLayout $ [whamlet|
                 <h1> Pedidos cadastrados:
                 $forall Entity peid pedido <- listaPed
                     <a href=@{ListaPedR peid}> #{pedidoNome pedido} 
                     <form method=post action=@{ListaPedR peid}> 
                         <input type="submit" value="Deletar"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

-}
postClienteR :: Handler Html
postClienteR = do
                ((result, _), _) <- runFormPost formCliente
                case result of
                    FormSuccess cliente -> do
                       runDB $ insert cliente 
                       defaultLayout [whamlet| 
                           <h1> #{clienteNome cliente} Inseridx com sucesso. 
                           <input name="" type="button" onClick="{/consulta/cliente}" value="Voltar">
                       |]
                    _ -> redirect ClienteR

postProdR :: Handler Html
postProdR = do
                ((result, _), _) <- runFormPost formProduto
                case result of
                    FormSuccess produto -> do
                       runDB $ insert produto 
                       defaultLayout [whamlet| 
                           <h1> #{produtoNome produto} Inserido com sucesso. 
                           <input name="" type="button" onClick="{/consulta/produto}" value="Voltar">
                       |]
                    _ -> redirect ProdR

postListaCliR :: ClienteId -> Handler Html
postListaCliR cid = do
     runDB $ delete cid
     redirect ListarR

postListaProdR :: ProdutoId -> Handler Html
postListaProdR pid = do
     runDB $ delete pid
     redirect ListProdR
     
getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]


getPedidoR :: Handler Html
getPedidoR = do
             (widget, enctype) <- generateFormPost formPedido
             defaultLayout $ widgetForm PedidoR enctype widget "Pedidos"

postPedidoR :: Handler Html
postPedidoR = do
                ((result, _), _) <- runFormPost formPedido
                case result of
                    FormSuccess pedido -> do
                       runDB $ insert pedido 
                       defaultLayout [whamlet| 
                           <h1> Inserido com sucesso. 
                       |]
                    _ -> redirect PedidoR
{-
getListaPedR :: PedidoId -> Handler Html
getListaPedR peid = do
             pedido <- runDB $ get404 peid 
             defaultLayout [whamlet| 
                 <h1> Dados do Pedido
                 <p> Cliente: #{pedidoNome pedido}
                 <p> Entrega: #{pedidoDataEntrega pedido}
                 <p> Valor: R$#{pedidoTotal pedido}
                 |]



postListaPedR :: PedidoId -> Handler Html
postListaPedR peid = do
     runDB $ delete peid
     redirect ListPedR
-} 
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