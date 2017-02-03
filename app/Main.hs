module Main where

import           Network.Wai.Handler.Warp (run)
import           Piserv.API
import           Piserv.Handlers
import           Servant                  (serve)
import           System.Environment
import           System.Hardware.WiringPi (wiringPiSetupGpio)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
       []  -> 8080
       [p] -> read p
       _   -> error "Please provide a single integer argument representing port, e.g. 8080)"
  wiringPiSetupGpio
  putStrLn "Piserv running on PORT=8080."
  run port (serve piApi piservServer)

