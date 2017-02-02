module Main where

import           Network.Wai.Handler.Warp (run)
import           Piserv.API
import           Piserv.Handlers
import           Servant                  (serve)
import           System.Hardware.WiringPi (wiringPiSetupGpio)

main :: IO ()
main = do
  wiringPiSetupGpio
  putStrLn "Piserv running on PORT=8080."
  run 8080 (serve piApi piservServer)

