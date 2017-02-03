module Main where

import           Data.Semigroup           ((<>))
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
  putStrLn ("Piserver running on PORT=" <> show port)
  run port (serve api server)

