{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Main where
import Import
import Yesod
import Foundation
import Handlers
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)


data Pagina = Pagina{connPool :: ConnectionPool}

-----fim do main


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


-- fim do main ------------------

connStr = "dbname=d5h7illvto4uf1 host=ec2-54-163-226-48.compute-1.amazonaws.com user=ewmpaizbvqiapn password=o2Qua4NTtZeviSPvjC7k_9LJ5c port=5432"


main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)