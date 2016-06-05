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
    telefone Int
    endereco Text
    cidade Text
    deriving Show

Produto json
    nome Text
    descricao Text
    valor Double
    deriving Show


Pedido json
    clienteid ClienteId
    --clientenome
    dataPed Day
    dataEntrega Day
    total Double
    deriving Show

Users json
   nome Text
   login Text
   senha Text
   deriving Show
 |]

mkYesodData "Pagina" pRoutes


mkMessage "Pagina" "messages" "pt-br"




type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage