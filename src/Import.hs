{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
/ HomeR GET 
/cadastro/cliente ClienteR GET POST
/cadastro/produto ProdR GET POST
/cadastro/usuario UsuarioR GET POST
/cadastro/fornecedor FornR GET POST
--   /hello HelloR GET
/consulta/cliente ListarR GET 
/consulta/produto ListProdR GET 
/consulta/fornecedores ListFornR GET
/consulta/usuario ListUserR GET
/consulta/cliente/#ClienteId ListaCliR GET POST
/consulta/produto/#ProdutoId ListaProdR GET POST
/consulta/fornecedor/#FornecedorId ListaFornR GET POST
/consulta/usuario/#UsersId ListaUserR GET POST
/perfil/#UsersId PerfilR GET
/erro ErroR GET
/login LoginR GET POST
/admin AdminR GET
/logout LogoutR GET
|]