{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Piserv.API where

import           Data.Proxy
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import           Data.Text.Read           (decimal)
import           Data.Word                (Word16, Word8)
import           Servant                  ((:<|>), (:>), Capture,
                                           FromHttpApiData (..), Get, NoContent,
                                           PlainText, PostAccepted,
                                           ToHttpApiData (..))
import           System.Hardware.WiringPi


data DigitalMode
  = DInput
  | DOutput
  deriving (Eq, Show)

newtype PinNumber = PinNumber Int deriving (Eq, Show)
newtype PudValue = PudValue Pud deriving (Eq, Show)
newtype PinValue = PinValue Value deriving (Eq, Show)
newtype PwmVal = PwmVal Word16 deriving (Eq, Show)
newtype PwmValRange = PwmValRange Word16 deriving (Eq, Show)
newtype PwmValClock = PwmValClock Word16 deriving (Eq, Show)
newtype PwmMde = PwmMde PwmMode deriving (Eq, Show)

instance FromHttpApiData DigitalMode where
  parseUrlPiece "input"  = pure DInput
  parseUrlPiece "output" = pure DOutput
  parseUrlPiece bad      = Left bad
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PinNumber where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> if n <= 26 && n > 0
                    then pure (PinNumber n)
                    else Left (num <> ": pin is out of bounds")
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PudValue where
  parseUrlPiece "pud_off"  = pure (PudValue PUD_OFF)
  parseUrlPiece "pud_up"   = pure (PudValue PUD_UP)
  parseUrlPiece "pud_down" = pure (PudValue PUD_DOWN)
  parseUrlPiece bad        = Left bad
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PinValue where
  parseUrlPiece "low"  = pure (PinValue LOW)
  parseUrlPiece "high" = pure (PinValue HIGH)
  parseUrlPiece bad    = Left bad
  parseQueryParam = parseUrlPiece

instance ToHttpApiData PinValue where
  toUrlPiece (PinValue LOW)  = "low"
  toUrlPiece (PinValue HIGH) = "high"
  toQueryParam = toUrlPiece

instance FromHttpApiData PwmVal where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> pure (PwmVal n)
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PwmValRange where
  parseUrlPiece num = case decimal num of
    Right (n, _)  -> if n <= 4096
                     then pure (PwmValRange n)
                     else Left (num <> ": is out of range, default = 1024, max 4096")
    Left _        -> Left num
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PwmValClock where
  parseUrlPiece num = case decimal num of
    Right (n, _)  -> if n >= 2 && n <= 4095
                     then pure (PwmValClock n)
                     else Left (num <> ": is out of range, min = 2, max 4095")
    Left _        -> Left num
  parseQueryParam = parseUrlPiece

instance FromHttpApiData PwmMde where
  parseUrlPiece "pwm_mode_bal" = pure (PwmMde PWM_MODE_BAL)
  parseUrlPiece "pwm_mode_ms"  = pure (PwmMde PWM_MODE_MS)
  parseUrlPiece bad            = Left bad
  parseQueryParam = parseUrlPiece

type API = "pinmode" :> "wpi" :> "1" :> "pwm_output" :> PostAccepted '[PlainText] NoContent
      :<|> "pinmode" :> "wpi" :> "7" :> "gpio_clock" :> PostAccepted '[PlainText] NoContent
      :<|> "pinmode" :> "wpi" :> Capture "wpiint" PinNumber :> Capture "wpimode" DigitalMode :> PostAccepted '[PlainText] NoContent
      :<|> "pinmode" :> "gpio" :> Capture "gpioint" PinNumber :> Capture "gpiomode" DigitalMode :> PostAccepted '[PlainText] NoContent
      :<|> "pinmode" :> "phys" :> Capture "physint" PinNumber :> Capture "physmode" DigitalMode :> PostAccepted '[PlainText] NoContent
      :<|> "pullupdncontrol" :> "wpi" :> Capture "wpiint" PinNumber :> Capture "wpipud" PudValue :> PostAccepted '[PlainText] NoContent
      :<|> "pullupdncontrol" :> "gpio" :> Capture "gpioint" PinNumber :> Capture "gpiopud" PudValue :> PostAccepted '[PlainText] NoContent
      :<|> "pullupdncontrol" :> "phys" :> Capture "physint" PinNumber :> Capture "physpud" PudValue :> PostAccepted '[PlainText] NoContent
      :<|> "digitalread" :> "wpi" :> Capture "wpiint" PinNumber :> Get '[PlainText] Text
      :<|> "digitalread" :> "gpio" :> Capture "gpioint" PinNumber :> Get '[PlainText] Text
      :<|> "digitalread" :> "phys" :> Capture "physint" PinNumber :> Get '[PlainText] Text
      :<|> "digitalwrite" :> "wpi" :> Capture "wpiint" PinNumber :> Capture "wpivalue" PinValue :> PostAccepted '[PlainText] NoContent
      :<|> "digitalwrite" :> "gpio" :> Capture "gpioint" PinNumber :> Capture "gpivalue" PinValue :> PostAccepted '[PlainText] NoContent
      :<|> "digitalwrite" :> "phys" :> Capture "physint" PinNumber :> Capture "physvalue" PinValue :> PostAccepted '[PlainText] NoContent
      :<|> "pwmwrite" :> "wpi" :> Capture "wpiint" PinNumber :> Capture "wpipwm" PwmVal :> PostAccepted '[PlainText] NoContent
      :<|> "pwmwrite" :> "gpio" :> Capture "gpioint" PinNumber :> Capture "gpipwm" PwmVal :> PostAccepted '[PlainText] NoContent
      :<|> "pwmwrite" :> "phys" :> Capture "physint" PinNumber :> Capture "physpwm" PwmVal :> PostAccepted '[PlainText] NoContent
      :<|> "digitalwritebytes" :> Capture "dwb" Word8 :> PostAccepted '[PlainText] NoContent
      :<|> "pwmsetmode" :> Capture "psm" PwmMde :> PostAccepted '[PlainText] NoContent
      :<|> "pwmsetrange" :> Capture "psr" PwmValRange :> PostAccepted '[PlainText] NoContent
      :<|> "pwmsetclock" :> Capture "psc" PwmValClock :> PostAccepted '[PlainText] NoContent
      :<|> "piboardrev" :> Get '[PlainText] Text
      :<|> "pintobcmgpio" :> "wpi" :> Capture "wpiint" PinNumber :> Get '[PlainText] Text
      :<|> "pintobcmgpio" :> "gpio" :> Capture "gpioint" PinNumber :> Get '[PlainText] Text
      :<|> "pintobcmgpio" :> "phys" :> Capture "physint" PinNumber :> Get '[PlainText] Text

piApi :: Proxy API
piApi = Proxy

