{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Piserv.Handlers where

import           Control.Exception          (IOException, catch)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Conversion (toByteString)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, pack)
import           Data.Word                  (Word8)
import           Piserv.API
import           Servant                    ((:<|>) (..), NoContent (..))
import           Servant.Server             (Handler, Server, err404, err500,
                                             errBody)
import           System.Hardware.WiringPi


piservServer :: Server API
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

valueToText
  :: Value
  -> Text
valueToText LOW  = "low"
valueToText HIGH = "high"

digitalReadWpi
  :: PinNumber
  -> Handler Text
digitalReadWpi (PinNumber n) =
  valueToText <$> runPIO (digitalRead (Wpi n))

digitalReadGpio
  :: PinNumber
  -> Handler Text
digitalReadGpio (PinNumber n) =
  valueToText <$> runPIO (digitalRead (Gpio n))

digitalReadPhys
  :: PinNumber
  -> Handler Text
digitalReadPhys (PinNumber n) =
  valueToText <$> runPIO (digitalRead (Phys n))

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
  :: Word8
  -> Handler NoContent
digitalWriteByte' w =
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

piBoardRev' :: Handler Text
piBoardRev' = pack . show <$> runPIO piBoardRev

pinToBcmGpioWpi
  :: PinNumber
  -> Handler Text
pinToBcmGpioWpi (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Wpi n)))
  case res of
    Just r  -> return (pack (show r))
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioGpio
  :: PinNumber
  -> Handler Text
pinToBcmGpioGpio (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Gpio n)))
  case res of
    Just r  -> return (pack (show r))
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

pinToBcmGpioPhys
  :: PinNumber
  -> Handler Text
pinToBcmGpioPhys (PinNumber n) = do
  res <- runPIO (return (pinToBcmGpio (Phys n)))
  case res of
    Just r  -> return (pack (show r))
    Nothing -> throwError (err404 { errBody = "Can't convert BCM to GPIO for pin: " <>
                                              toByteString (show n) })

