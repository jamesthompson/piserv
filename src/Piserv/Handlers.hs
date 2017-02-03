{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Piserv.Handlers where

import           Control.Exception          (IOException, catch)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Conversion (toByteString)
import           Data.Semigroup             ((<>))
import           Network.HTTP.Types.Status  (ok200)
import           Network.Wai                (responseLBS)
import           Piserv.API
import           Servant                    ((:<|>) (..), NoContent (..))
import           Servant.Server             (Handler, Server, err404, err500,
                                             errBody)
import           System.Hardware.WiringPi


piservServer :: Server PIAPI
piservServer = pinModePwmOutput
          :<|> pinModeGpioClock
          :<|> pinModeWpi
          :<|> pinModeGpio
          :<|> pinModePhys
          :<|> pullUpDnControlWpi
          :<|> pullUpDnControlGpio
          :<|> pullUpDnControlPhys
          :<|> digitalReadWpi
          :<|> digitalReadGpio
          :<|> digitalReadPhys
          :<|> digitalWriteWpi
          :<|> digitalWriteGpio
          :<|> digitalWritePhys
          :<|> pwmWriteWpi
          :<|> pwmWriteGpio
          :<|> pwmWritePhys
          :<|> digitalWriteByte'
          :<|> pwmSetMode'
          :<|> pwmSetRange'
          :<|> pwmSetClock'
          :<|> piBoardRev'
          :<|> pinToBcmGpioWpi
          :<|> pinToBcmGpioGpio
          :<|> pinToBcmGpioPhys
          :<|> health
          :<|> serveDocs
  where serveDocs _ respond =
          respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

runPIO
  :: IO a
  -> Handler a
runPIO action = do
    res <- liftIO $ catch (pure <$> action) excp
    case res of
      Right a -> pure a
      Left e  -> throwError e
  where excp a = pure (Left (err500 { errBody = toByteString (show (a :: IOException)) } ))

pinModePwmOutput :: Handler NoContent
pinModePwmOutput = runPIO (pinMode (Wpi 1) PWM_OUTPUT) >> return NoContent

pinModeGpioClock :: Handler NoContent
pinModeGpioClock = runPIO (pinMode (Wpi 7) GPIO_CLOCK) >> return NoContent

pinModeWpi
  :: PinNumber
  -> DigitalMode
  -> Handler NoContent
pinModeWpi (PinNumber n) DInput  = runPIO (pinMode (Wpi n) INPUT) >> return NoContent
pinModeWpi (PinNumber n) DOutput = runPIO (pinMode (Wpi n) OUTPUT) >> return NoContent

pinModeGpio
  :: PinNumber
  -> DigitalMode
  -> Handler NoContent
pinModeGpio (PinNumber n) DInput  = runPIO (pinMode (Gpio n) INPUT) >> return NoContent
pinModeGpio (PinNumber n) DOutput = runPIO (pinMode (Gpio n) OUTPUT) >> return NoContent

pinModePhys
  :: PinNumber
  -> DigitalMode
  -> Handler NoContent
pinModePhys (PinNumber n) DInput  = runPIO (pinMode (Phys n) INPUT) >> return NoContent
pinModePhys (PinNumber n) DOutput = runPIO (pinMode (Phys n) OUTPUT) >> return NoContent

pullUpDnControlWpi
  :: PinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlWpi (PinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Wpi n) v) >> return NoContent

pullUpDnControlGpio
  :: PinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlGpio (PinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Gpio n) v) >> return NoContent

pullUpDnControlPhys
  :: PinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlPhys (PinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Phys n) v) >> return NoContent

digitalReadWpi
  :: PinNumber
  -> Handler Value
digitalReadWpi (PinNumber n) =
  runPIO (digitalRead (Wpi n))

digitalReadGpio
  :: PinNumber
  -> Handler Value
digitalReadGpio (PinNumber n) =
  runPIO (digitalRead (Gpio n))

digitalReadPhys
  :: PinNumber
  -> Handler Value
digitalReadPhys (PinNumber n) =
  runPIO (digitalRead (Phys n))

digitalWriteWpi
  :: PinNumber
  -> PinValue
  -> Handler NoContent
digitalWriteWpi (PinNumber n) (PinValue v) =
  runPIO (digitalWrite (Wpi n) v) >> return NoContent

digitalWriteGpio
  :: PinNumber
  -> PinValue
  -> Handler NoContent
digitalWriteGpio (PinNumber n) (PinValue v) =
  runPIO (digitalWrite (Gpio n) v) >> return NoContent

digitalWritePhys
  :: PinNumber
  -> PinValue
  -> Handler NoContent
digitalWritePhys (PinNumber n) (PinValue v) =
  runPIO (digitalWrite (Phys n) v) >> return NoContent

pwmWriteWpi
  :: PinNumber
  -> PwmVal
  -> Handler NoContent
pwmWriteWpi (PinNumber n) (PwmVal v) =
  runPIO (pwmWrite (Wpi n) v) >> return NoContent

pwmWriteGpio
  :: PinNumber
  -> PwmVal
  -> Handler NoContent
pwmWriteGpio (PinNumber n) (PwmVal v) =
  runPIO (pwmWrite (Gpio n) v) >> return NoContent

pwmWritePhys
  :: PinNumber
  -> PwmVal
  -> Handler NoContent
pwmWritePhys (PinNumber n) (PwmVal v) =
  runPIO (pwmWrite (Phys n) v) >> return NoContent

digitalWriteByte'
  :: DigByte
  -> Handler NoContent
digitalWriteByte' (DigByte w) =
  runPIO (digitalWriteByte w) >> return NoContent

pwmSetMode'
  :: PwmMde
  -> Handler NoContent
pwmSetMode' (PwmMde m) =
  runPIO (pwmSetMode m) >> return NoContent

pwmSetRange'
  :: PwmValRange
  -> Handler NoContent
pwmSetRange' (PwmValRange r) =
  runPIO (pwmSetRange r) >> return NoContent

pwmSetClock'
  :: PwmValClock
  -> Handler NoContent
pwmSetClock' (PwmValClock c) =
  runPIO (pwmSetClock c) >> return NoContent

piBoardRev' :: Handler Int
piBoardRev' = runPIO piBoardRev

pinToBcmGpioWpi
  :: PinNumber
  -> Handler Int
pinToBcmGpioWpi (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Wpi n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioGpio
  :: PinNumber
  -> Handler Int
pinToBcmGpioGpio (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Gpio n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioPhys
  :: PinNumber
  -> Handler Int
pinToBcmGpioPhys (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Phys n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

health :: Handler Health
health = return OK

