{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
   / CadastroR GET POST
   /hello HelloR GET
   /listar ListarR GET 
   /pessoa/#PessoaId PessoaR GET POST
   /depto DeptoR GET POST
|]