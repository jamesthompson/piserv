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


server :: Server DOCSAPI
server = piservServer :<|> serveDocs
  where serveDocs _ respond =
          respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

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
  :: WpiPinNumber
  -> DigitalMode
  -> Handler NoContent
pinModeWpi (WpiPinNumber n) DInput  = runPIO (pinMode (Wpi n) INPUT) >> return NoContent
pinModeWpi (WpiPinNumber n) DOutput = runPIO (pinMode (Wpi n) OUTPUT) >> return NoContent

pinModeGpio
  :: GpioPinNumber
  -> DigitalMode
  -> Handler NoContent
pinModeGpio (GpioPinNumber n) DInput  = runPIO (pinMode (Gpio n) INPUT) >> return NoContent
pinModeGpio (GpioPinNumber n) DOutput = runPIO (pinMode (Gpio n) OUTPUT) >> return NoContent

pinModePhys
  :: PhysPinNumber
  -> DigitalMode
  -> Handler NoContent
pinModePhys (PhysPinNumber n) DInput  = runPIO (pinMode (Phys n) INPUT) >> return NoContent
pinModePhys (PhysPinNumber n) DOutput = runPIO (pinMode (Phys n) OUTPUT) >> return NoContent

pullUpDnControlWpi
  :: WpiPinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlWpi (WpiPinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Wpi n) v) >> return NoContent

pullUpDnControlGpio
  :: GpioPinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlGpio (GpioPinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Gpio n) v) >> return NoContent

pullUpDnControlPhys
  :: PhysPinNumber
  -> PudValue
  -> Handler NoContent
pullUpDnControlPhys (PhysPinNumber n) (PudValue v) =
  runPIO (pullUpDnControl (Phys n) v) >> return NoContent

digitalReadWpi
  :: WpiPinNumber
  -> Handler Value
digitalReadWpi (WpiPinNumber n) =
  runPIO (digitalRead (Wpi n))

digitalReadGpio
  :: GpioPinNumber
  -> Handler Value
digitalReadGpio (GpioPinNumber n) =
  runPIO (digitalRead (Gpio n))

digitalReadPhys
  :: PhysPinNumber
  -> Handler Value
digitalReadPhys (PhysPinNumber n) =
  runPIO (digitalRead (Phys n))

digitalWriteWpi
  :: WpiPinNumber
  -> PinValue
  -> Handler NoContent
digitalWriteWpi (WpiPinNumber n) (PinValue v) =
  runPIO (digitalWrite (Wpi n) v) >> return NoContent

digitalWriteGpio
  :: GpioPinNumber
  -> PinValue
  -> Handler NoContent
digitalWriteGpio (GpioPinNumber n) (PinValue v) =
  runPIO (digitalWrite (Gpio n) v) >> return NoContent

digitalWritePhys
  :: PhysPinNumber
  -> PinValue
  -> Handler NoContent
digitalWritePhys (PhysPinNumber n) (PinValue v) =
  runPIO (digitalWrite (Phys n) v) >> return NoContent

pwmWriteWpi
  :: WpiPinNumber
  -> PwmVal
  -> Handler NoContent
pwmWriteWpi (WpiPinNumber n) (PwmVal v) =
  runPIO (pwmWrite (Wpi n) v) >> return NoContent

pwmWriteGpio
  :: GpioPinNumber
  -> PwmVal
  -> Handler NoContent
pwmWriteGpio (GpioPinNumber n) (PwmVal v) =
  runPIO (pwmWrite (Gpio n) v) >> return NoContent

pwmWritePhys
  :: PhysPinNumber
  -> PwmVal
  -> Handler NoContent
pwmWritePhys (PhysPinNumber n) (PwmVal v) =
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
  :: WpiPinNumber
  -> Handler Int
pinToBcmGpioWpi (WpiPinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Wpi n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioGpio
  :: GpioPinNumber
  -> Handler Int
pinToBcmGpioGpio (GpioPinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Gpio n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioPhys
  :: PhysPinNumber
  -> Handler Int
pinToBcmGpioPhys (PhysPinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Phys n)))
  case res of
    Just r  -> return r
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

health :: Handler Health
health = return OK

