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
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Form.Bootstrap3


import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes



getHomeR :: Handler Html
getHomeR = defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/index.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
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
    isAuthorized ClienteR _ = return Authorized
    isAuthorized ProdR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized ListarR _ = return Authorized
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
             areq textField "Telefone" Nothing <*>
             areq textField "Endereco" Nothing <*>
             areq textField "Cidade" Nothing
        

formProduto :: Form Produto
formProduto = renderDivs $ Produto <$>
             areq textField "Nome" Nothing <*>
             areq textareaField "Descrição" Nothing <*>
             areq doubleField "Valor" Nothing
             
formFornecedor :: Form Fornecedor
formFornecedor = renderDivs $ Fornecedor <$>
             areq textField "Nome" Nothing <*>
             areq textField "Telefone" Nothing <*>
             areq textField "Endereco" Nothing <*>
             areq textField "Cidade" Nothing
             

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
            defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/addclientes.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]  

getProdR :: Handler Html
getProdR = do
            (widget, enctype) <- generateFormPost formProduto
            defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/addproduto.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]  

getFornR :: Handler Html
getFornR = do
            (widget, enctype) <- generateFormPost formFornecedor
            defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/addforn.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]  


getListaCliR :: ClienteId -> Handler Html
getListaCliR cid = do
        cliente <- runDB $ get404 cid 
        defaultLayout $ do
            menu <- widgetMenu
            toWidget $ $(luciusFile "templates/style.lucius")
            $(whamletFile "templates/cliente.hamlet")
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
            toWidgetHead
                [hamlet|
                    <meta charset="UTF-8">  
                |]


getListaProdR :: ProdutoId -> Handler Html
getListaProdR pid = do
            produto <- runDB $ get404 pid 
            defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/produto.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]


--Tela de consulta de clientes cadastrados
getListarR :: Handler Html
getListarR = do
             listaC <- runDB $ selectList [] [Asc ClienteNome]
             defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/verclientes.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
                addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
                addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                <meta charset="UTF-8">  
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]


getListProdR :: Handler Html
getListProdR = do
             listaP <- runDB $ selectList [] [Asc ProdutoNome]
             defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/verprodutos.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.min.css"
                addScriptRemote "https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"
                addScriptRemote "https://cdn.datatables.net/1.10.12/js/dataTables.bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                <meta charset="UTF-8">  
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
                       defaultLayout $ do
                            menu <- widgetMenu
                            toWidget $ $(luciusFile "templates/style.lucius")
                            $(whamletFile "templates/novocad.hamlet")
                            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                            toWidgetHead
                                [hamlet|
                                    <meta charset="UTF-8">  
                                |]
                    _ -> redirect ClienteR

postProdR :: Handler Html
postProdR = do
                ((result, _), _) <- runFormPost formProduto
                case result of
                    FormSuccess produto -> do
                       runDB $ insert produto 
                       defaultLayout $ do
                            menu <- widgetMenu
                            toWidget $ $(luciusFile "templates/style.lucius")
                            $(whamletFile "templates/novocad.hamlet")
                            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                            toWidgetHead
                                [hamlet|
                                    <meta charset="UTF-8">  
                                |]
                    _ -> redirect ProdR

postFornR :: Handler Html
postFornR = do
                ((result, _), _) <- runFormPost formFornecedor
                case result of
                    FormSuccess fornecedor -> do
                       runDB $ insert fornecedor 
                       defaultLayout $ do
                            menu <- widgetMenu
                            toWidget $ $(luciusFile "templates/style.lucius")
                            $(whamletFile "templates/novocad.hamlet")
                            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                            toWidgetHead
                                [hamlet|
                                    <meta charset="UTF-8">  
                                |]
                    _ -> redirect FornR

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
           (widget, enctype) <- generateFormPost formUsers
           defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/cadastro.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]



formUsers :: Form Users
formUsers = renderBootstrap3 BootstrapBasicForm $ Users <$>
           areq textField (bfs ("Nome" :: Text)) Nothing <*>
           areq textField (bfs ("Login" :: Text)) Nothing <*>
           areq passwordField (bfs ("Senha" :: Text)) Nothing


formLogin :: Form (Text,Text)
formLogin = renderBootstrap3 BootstrapBasicForm $ (,) <$>
           areq textField (bfs ("Login" :: Text)) Nothing <*>
           areq passwordField (bfs ("Senha" :: Text)) Nothing



getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{usersNome user}
          <p><b> Login: #{usersLogin user}
      |]
      
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUsers
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
             defaultLayout $ do
                menu <- widgetMenu
                toWidget $ $(luciusFile "templates/style.lucius")
                $(whamletFile "templates/login.hamlet")
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"    
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "0" >> redirect HomeR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (HomeR)


getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     redirect (HomeR)

     
getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]


widgetMenu = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing ->  [hamlet|
        <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{ClienteR}"><i class="fa fa-user-plus" aria-hidden="true"></i> Cadastro de Clientes</a><p>
                    <li><a href="@{ProdR}"><i class="fa fa-barcode" aria-hidden="true"></i> Cadastro de Produtos</a><p>
                    <li><a href="@{FornR}"><i class="fa fa-cart-plus" aria-hidden="true"></i> Cadastro de Fornecedor</a><p>
                    <li><a href="@{ListarR}"><i class="fa fa-users" aria-hidden="true"></i> Clientes Cadastrados</a><p>
                    <li><a href="@{ListProdR}"><i class="fa fa-list-ul" aria-hidden="true"></i> Produtos Cadastrados</a><p>
                    <li><a href="@{LoginR}"><i class="fa fa-sign-in fa-fw" aria-hidden="true"></i> Login</a>
    |]
        Just "0" -> [hamlet|
            <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{ClienteR}"><i class="fa fa-user-plus" aria-hidden="true"></i> Cadastro de Clientes</a><p>
                    <li><a href="@{ProdR}"><i class="fa fa-barcode" aria-hidden="true"></i> Cadastro de Produtos</a><p>
                    <li><a href="@{FornR}"><i class="fa fa-cart-plus" aria-hidden="true"></i> Cadastro de Fornecedor</a><p>
                    <li><a href="@{ListarR}"><i class="fa fa-users" aria-hidden="true"></i> Clientes Cadastrados</a><p>
                    <li><a href="@{ListProdR}"><i class="fa fa-list-ul" aria-hidden="true"></i> Produtos Cadastrados</a><p>
                    <li><a href="@{LogoutR}"><i class="fa fa-sign-out fa-fw" aria-hidden="true"></i> Logout</a>
                    <li><a href="@{AdminR}"><i class="fa fa-lock fa-fw" aria-hidden="true"></i> Administração</a>
                    |]
        Just _ -> [hamlet|
            <nav id="column_left">
                <ul class="nav nav-list">
                    <li><a href="@{HomeR}"><i class="fa fa-home fa-fw" aria-hidden="true"></i> Home</a>
                    <li><a href="@{ClienteR}"><i class="fa fa-user-plus" aria-hidden="true"></i> Cadastro de Clientes</a><p>
                    <li><a href="@{ProdR}"><i class="fa fa-barcode" aria-hidden="true"></i> Cadastro de Produtos</a><p>
                    <li><a href="@{FornR}"><i class="fa fa-cart-plus" aria-hidden="true"></i> Cadastro de Fornecedor</a><p>
                    <li><a href="@{ListarR}"><i class="fa fa-users" aria-hidden="true"></i> Clientes Cadastrados</a><p>
                    <li><a href="@{ListProdR}"><i class="fa fa-list-ul" aria-hidden="true"></i> Produtos Cadastrados</a><p>
                    <li><a href="@{LogoutR}"><i class="fa fa-sign-out fa-fw" aria-hidden="true"></i> Logout</a>
                    |]