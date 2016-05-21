{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)


data Pagina = Pagina{connPool :: ConnectionPool}


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clientes json
   nome Text
   deriving Show


Users json
   nome Text
   login Text
   senha Text
   deriving Show
   
   
Produtoz json
    nome Text
    valor Double
    deriving Show


Aluno json
    nome Text
    ra Text
    cel Text 
    idade Text
    deriving Show
    
ClientesProdutos json
    clid ClientesId
    prid ProdutozId
    alid AlunoId
    UniqueClientesProdutos clid prid alid
    --unique serve só pra restringir o cadastro do mesmo cliente pro mesmo produto
|]


mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/cadastro UserR GET POST OPTIONS
/listar/cliente/#ClientesId ListaR GET OPTIONS
/action/cliente/#ClientesId ActionR PUT DELETE
/produto/cadastro ProdutoR GET POST
/produto/checar/#ProdutozId ChecarProdR GET
/aluno/cadastro AlunoR GET POST
/aluno/checar/#AlunoId ChecarAlunoR GET
/venda VendaR POST
/venda/check/#ClientesId VendaCliR GET
/consulta ConsultaR GET
/erro ErroR GET
/login LoginR GET
/usuario UsuarioR GET POST
/perfil/#UsersId PerfilR GET
/admin AdminR GET
/logout LogoutR GET
|]



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


instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
       

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------------------------------------

--Aula 14

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

      |]

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

--aula 13

-- Sempre que preciso um form, sera ncessario
-- funcoes deste tipo
formProd :: Form Produtoz
formProd = renderDivs $ Produtoz <$>
           areq textField "Nome: " Nothing <*>
           areq doubleField "Valor: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProd
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:red;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{ProdutoR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

postProdutoR :: Handler Html
postProdutoR = do
           ((result, _), _) <- runFormPost formProd
           case result of 
               FormSuccess prod -> (runDB $ insert prod) >>= \piid -> redirect (ChecarProdR piid)
               _ -> redirect ErroR
           


getChecarProdR :: ProdutozId -> Handler Html
getChecarProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> #{produtozNome produto}  
        <p><b> #{show $ produtozValor produto}
    |]


--exercicio 2
formAlun :: Form Aluno
formAlun = renderDivs $ Aluno <$>
            areq textField "Nome: " Nothing <*>
            areq textField "RA: " Nothing <*>
            areq textField "Cel: " Nothing <*>
            areq textField "Idade: " Nothing

getAlunoR :: Handler Html
getAlunoR = do
           (widget, enctype) <- generateFormPost formAlun
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:blue;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{AlunoR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

postAlunoR :: Handler Html
postAlunoR = do
           ((result, _),_) <- runFormPost formAlun
           case result of 
               FormSuccess aluno -> (runDB $ insert aluno) >>= \piid -> redirect (ChecarAlunoR piid)
               _ -> redirect ErroR
           


getChecarAlunoR :: AlunoId -> Handler Html
getChecarAlunoR aid = do
    aluno <- runDB $ get404 aid
    defaultLayout [whamlet|
        <p><b> #{alunoNome aluno}  
        <p><b> #{alunoRa aluno}
        <p><b> #{alunoCel aluno}
        <p><b> #{alunoIdade aluno}
    |]


getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]


getListaR :: ClientesId -> Handler()
getListaR cid = do
    cliente <- runDB $ get404 cid
    sendResponse $ toJSON cliente


optionsListaR :: ClientesId -> Handler()
optionsListaR _ = addHeader "Acess-Control-Allow-Methods" "GET"

deleteActionR :: ClientesId -> Handler()
deleteActionR pid = do
    runDB $ delete pid
    sendResponse (object [pack "resp" .= pack "DELETED"])


putActionR :: ClientesId -> Handler ()
putActionR cid = do
    cli <- requireJsonBody :: Handler Clientes
    runDB $ update cid [ClientesNome =. clientesNome cli]
    sendResponse (object [pack "resp" .= pack "UPDATED"])


getUserR :: Handler Html
getUserR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <form>
        Nome: <input type="text" id="usuario"> <br>
    <button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{UserR}",
                 type: "POST",
                 data:  JSON.stringify({"nome":$("#usuario").val(),}),
                 
                 success: function(data) {
                     alert(data.resp);
                     $("#usuario").val("");
                  
                 }
            })
         });
     }
  |]
  
  -- PRECISO TRANSFORMAR cliente em JSON e retornar
  

postUserR :: Handler ()
postUserR = do
    cliente <- requireJsonBody :: Handler Clientes 
    runDB $ insert cliente
    sendResponse (object [pack "resp" .= pack "CREATED"])
      
    -- Linha 60: Le o json {nome:"Teste"} e converte para
    -- Clientes "Teste". 
    -- O comando runDB $ insert (Clientes "Teste")
    -- Insere o registro "Teste" no banco
    -- {resp:"CREATED"}

optionsUserR :: Handler()
optionsUserR = addHeader "Acess-Control-Allow-Methods" "GET POST"

getConsultaR :: Handler ()
getConsultaR = do
    allClientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allClientes])


--getProdutoR :: Handler ()
--getProdutoR = do
--    allProd <- runDB $ selectList [] [Asc ProdutozValor]
--    sendResponse (object [pack "data" .= fmap toJSON allProd])
    
--postProdutoR :: Handler ()
--postProdutoR = do
    --prod <- requireJsonBody :: Handler Produtoz
    --runDB $ insert prod
    --sendResponse (object [pack "resp" .= pack "CREATED"])
    
postVendaR :: Handler ()
postVendaR = do
    venda <- requireJsonBody :: Handler ClientesProdutos
    runDB $ insert venda
    sendResponse (object [pack "resp" .= pack "CREATED"])

getVendaCliR :: ClientesId -> Handler ()
getVendaCliR pid = do
    xs <- runDB $ (rawSql (pack $ "SELECT ??, ??, ?? FROM produtoz \
     INNER JOIN clientes_produtos ON produtoz.id=clientes_produtos.prid \
     INNER JOIN clientes ON clientes.id=clientes_produtos.clid \
     WHERE clientes_produtos.clid = " ++ (show $ fromSqlKey pid)) []) :: Handler [(Entity Produtoz,Entity ClientesProdutos,Entity Clientes)]
    sendResponse (object [pack "data" .= fmap (toJSON . (\(p,_,_) -> p)) xs])


connStr = "dbname=d5h7illvto4uf1 host=ec2-54-163-226-48.compute-1.amazonaws.com user=ewmpaizbvqiapn password=o2Qua4NTtZeviSPvjC7k_9LJ5c port=5432"

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Controle du capeta
    <p>
    <a href=@{ConsultaR}>Lista de clientes organizada por nome
    <p>
    <a href=@{UserR}>Cadastro de clientes
    <p>
    <a href=@{ProdutoR}>Cadastro de produtos
    <p>
    <a href=@{AlunoR}>Cadastro de alunos
 
|] 

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)