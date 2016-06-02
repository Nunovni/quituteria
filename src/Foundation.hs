{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Pagina = Pagina { connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Cliente json
    nome Text
    telefone Text
    preposto Text
    email Text
    endereco Text
    bairro Text
    referencia Text
    indicacao Text
    deriving Show


Users json
   nome Text
   login Text
   senha Text
   deriving Show
 

Produtos json
    nome Text
    descricao Text
    valor Double
    deriving Show


Pedido json
    nome Text
    --clientenome
    telefone Text
    --cliente telefone
    endentrega Text
    referencia Text
    dataPed Date
    dataEntrega Date
    total Double
    deriving Show
    
ProdutoPedido json
    idPedido Text
    nome Text
    --ProdutoNome
    valor Double
    --produtoValor
    total Double
    deriving Show

--ver se funfa do jeito abaixo
ClientesProdutos json
    clid ClientesId
    prid ProdutozId
    alid AlunoId
    UniqueClientesProdutos clid prid alid
    --unique serve só pra restringir o cadastro do mesmo cliente pro mesmo produto
|]

mkYesodData "Pagina" pRoutes


mkMessage "Pagina" "messages" "pt-br"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Pagina where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage