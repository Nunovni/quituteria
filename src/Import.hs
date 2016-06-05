{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
/ HomeR GET 
--/cadastro/ UserR GET POST OPTIONS
/cadastro/cliente ClienteR GET POST
/cadastro/usuario UsuarioR GET POST
/consulta/cliente/#ClienteId ListaCliR GET POST
--   / CadastroR GET POST
--   /hello HelloR GET
--
/consulta/cliente ListarR GET 
--   /pessoa/#PessoaId PessoaR GET POST
--   /depto DeptoR GET POST
/perfil/#UsersId PerfilR GET
/erro ErroR GET
/login LoginR GET POST
/admin AdminR GET
/logout LogoutR GET
|]