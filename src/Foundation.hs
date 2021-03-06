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
    endereco Text
    cidade Text
    deriving Show

Produto json
    nome Text
    descricao Textarea
    valor Double
    deriving Show


Fornecedor json
    nome Text
    telefone Text
    endereco Text
    cidade Text
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