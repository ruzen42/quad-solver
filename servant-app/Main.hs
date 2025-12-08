{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Servant
import SolveQuad
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Starting server on port 8082"
    --run 8082 $ serve (Proxy :: Proxy API) server

type API =
    "quad" :> ReqBody '[JSON] Equation :> Post '[JSON] EquationSolution

server :: Server API
server = solveHandler

solveHandler :: Equation -> Handler EquationSolution
solveHandler eq = return $ solve eq

