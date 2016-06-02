{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
/ HomeR GET 
/cadastro/ UserR GET POST OPTIONS
/cadastro/cliente ClienteR GET POST
/cadastro/produto ProdutoR GET POST
/cadastro/pedido PedidoR GET POST
/cadastro/usuario UsuarioR GET POST
/listar/cliente/#ClientesId ListaR GET OPTIONS
/action/cliente/#ClientesId ActionR PUT DELETE
/produto/checar/#ProdutozId ChecarProdR GET
/aluno/cadastro AlunoR GET POST
/aluno/checar/#AlunoId ChecarAlunoR GET
/venda VendaR POST
/venda/check/#ClientesId VendaCliR GET
/consulta ConsultaR GET
/erro ErroR GET
/login LoginR GET
/perfil/#UsersId PerfilR GET
/admin AdminR GET
/logout LogoutR GET
|]