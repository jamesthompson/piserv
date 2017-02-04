{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Piserv.API where

import           Data.ByteString.Conversion (toByteString)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Proxy
import           Data.Semigroup             ((<>))
import           Data.Text.Lazy             (pack)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Data.Text.Read             (decimal)
import           Data.Word                  (Word16, Word8)
import           Servant                    ((:<|>), (:>), Accept, Capture,
                                             FromHttpApiData (..), Get,
                                             MimeRender (..), NoContent,
                                             PlainText, PostAccepted, Raw,
                                             ToHttpApiData (..))
import           Servant.Docs
import           System.Hardware.WiringPi


data DigitalMode
  = DInput
  | DOutput
  deriving (Eq, Show)

newtype WpiPinNumber = WpiPinNumber Int deriving (Eq, Show)
newtype GpioPinNumber = GpioPinNumber Int deriving (Eq, Show)
newtype PhysPinNumber = PhysPinNumber Int deriving (Eq, Show)
newtype PudValue = PudValue Pud deriving (Eq, Show)
newtype PinValue = PinValue Value deriving (Eq, Show)
newtype PwmVal = PwmVal Word16 deriving (Eq, Show)
newtype PwmValRange = PwmValRange Word16 deriving (Eq, Show)
newtype PwmValClock = PwmValClock Word16 deriving (Eq, Show)
newtype PwmMde = PwmMde PwmMode deriving (Eq, Show)
newtype DigByte = DigByte Word8 deriving (Eq, Show)

instance FromHttpApiData DigitalMode where
  parseUrlPiece "input"  = pure DInput
  parseUrlPiece "output" = pure DOutput
  parseUrlPiece bad      = Left bad
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "digitalMode" DigitalMode) where
  toCapture _ =
    DocCapture "digitalMode"
               "(string) input || output"

instance ToSample DigitalMode where
  toSamples _ =
    [ ("When the 'digitalMode' is input", DInput)
    , ("When the 'digitalMode' is output", DOutput) ]

instance FromHttpApiData WpiPinNumber where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> if n >= 0 && n <= 31
                    then pure (WpiPinNumber n)
                    else Left (num <> ": pin is out of bounds")
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance ToSample WpiPinNumber where
  toSamples _ = singleSample (WpiPinNumber 1)

instance ToCapture (Capture "wpiPinNumber" WpiPinNumber) where
  toCapture _ =
    DocCapture "wpiPinNumber"
               "(int) 0 <= n <= 31"

instance FromHttpApiData GpioPinNumber where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> if n >= 0 && n <= 31
                    then pure (GpioPinNumber n)
                    else Left (num <> ": pin is out of bounds")
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance ToSample GpioPinNumber where
  toSamples _ = singleSample (GpioPinNumber 1)

instance ToCapture (Capture "gpioPinNumber" GpioPinNumber) where
  toCapture _ =
    DocCapture "gpioPinNumber"
               "(int) 0 <= n <= 31"

instance FromHttpApiData PhysPinNumber where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> case (n :: Int) of
      3  -> pure (PhysPinNumber 3)
      5  -> pure (PhysPinNumber 5)
      7  -> pure (PhysPinNumber 7)
      8  -> pure (PhysPinNumber 8)
      10 -> pure (PhysPinNumber 10)
      11 -> pure (PhysPinNumber 11)
      12 -> pure (PhysPinNumber 12)
      13 -> pure (PhysPinNumber 13)
      15 -> pure (PhysPinNumber 15)
      16 -> pure (PhysPinNumber 16)
      18 -> pure (PhysPinNumber 18)
      19 -> pure (PhysPinNumber 19)
      21 -> pure (PhysPinNumber 21)
      22 -> pure (PhysPinNumber 22)
      23 -> pure (PhysPinNumber 23)
      24 -> pure (PhysPinNumber 24)
      26 -> pure (PhysPinNumber 26)
      27 -> pure (PhysPinNumber 27)
      28 -> pure (PhysPinNumber 28)
      29 -> pure (PhysPinNumber 29)
      31 -> pure (PhysPinNumber 31)
      32 -> pure (PhysPinNumber 32)
      33 -> pure (PhysPinNumber 33)
      35 -> pure (PhysPinNumber 35)
      36 -> pure (PhysPinNumber 36)
      37 -> pure (PhysPinNumber 37)
      38 -> pure (PhysPinNumber 38)
      40 -> pure (PhysPinNumber 40)
      51 -> pure (PhysPinNumber 51)
      52 -> pure (PhysPinNumber 52)
      53 -> pure (PhysPinNumber 53)
      54 -> pure (PhysPinNumber 54)
      _  -> Left (num <> ": pin is out of bounds")
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance ToSample PhysPinNumber where
  toSamples _ = singleSample (PhysPinNumber 3)

instance ToCapture (Capture "physPinNumber" PhysPinNumber) where
  toCapture _ =
    DocCapture "physPinNumber"
               ("(int) 3, 5, 7, 8, 10,"    <>
                " 11, 12, 13, 15, 16, 18," <>
                " 19, 21, 22, 23, 24, 26," <>
                " 27, 28, 29, 31, 32, 33," <>
                " 35, 36, 37, 38, 40, 51," <>
                " 52, 53, 54")

instance FromHttpApiData PudValue where
  parseUrlPiece "pud_off"  = pure (PudValue PUD_OFF)
  parseUrlPiece "pud_up"   = pure (PudValue PUD_UP)
  parseUrlPiece "pud_down" = pure (PudValue PUD_DOWN)
  parseUrlPiece bad        = Left bad
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pudValue" PudValue) where
  toCapture _ =
    DocCapture "pudValue"
               "(string) pud_off || pud_up || pud_down"

instance ToSample PudValue where
  toSamples _ =
    [ ("When the 'pudValue' is pud_off", PudValue PUD_OFF)
    , ("When the 'pudValue' is pud_up", PudValue PUD_UP)
    , ("When the 'pudValue' is pud_down", PudValue PUD_DOWN) ]

instance FromHttpApiData PinValue where
  parseUrlPiece "low"  = pure (PinValue LOW)
  parseUrlPiece "high" = pure (PinValue HIGH)
  parseUrlPiece bad    = Left bad
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pinValue" PinValue) where
  toCapture _ =
    DocCapture "pinValue"
               "(string) low || high"

instance ToSample PinValue where
  toSamples _ =
    [ ("When the 'pinValue' is low", PinValue LOW)
    , ("When the 'pinValue' is high", PinValue HIGH) ]

instance ToHttpApiData PinValue where
  toUrlPiece (PinValue LOW)  = "low"
  toUrlPiece (PinValue HIGH) = "high"
  toQueryParam = toUrlPiece

instance FromHttpApiData PwmVal where
  parseUrlPiece num = case decimal num of
    Right (n, _) -> if n >= 0 && n <=4096
                    then pure (PwmVal n)
                    else Left (num <> ": pwm value is out of maximum range 0 <= n <= 4096")
    Left _       -> Left num
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pwmValue" PwmVal) where
  toCapture _ =
    DocCapture "pwmValue"
               "(int) default range = 0 <= n <= 1024, but range can go to 4096 by calling pwmsetrange"

instance ToSample PwmVal where
  toSamples _ = singleSample (PwmVal 1024)

instance FromHttpApiData PwmValRange where
  parseUrlPiece num = case decimal num of
    Right (n, _)  -> if n >= 0 && n <= 4096
                     then pure (PwmValRange n)
                     else Left (num <> ": is out of range, default max = 1024, absolute max 4096")
    Left _        -> Left num
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pwmRange" PwmValRange) where
  toCapture _ =
    DocCapture "pwmRange"
               "(int) 0 <= n <= 4096"

instance ToSample PwmValRange where
  toSamples _ = singleSample (PwmValRange 1024)

instance FromHttpApiData PwmValClock where
  parseUrlPiece num = case decimal num of
    Right (n, _)  -> if n >= 2 && n <= 4095
                     then pure (PwmValClock n)
                     else Left (num <> ": is out of range, min = 2, max 4095")
    Left _        -> Left num
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pwmClock" PwmValClock) where
  toCapture _ =
    DocCapture "pwmClock"
               "(int) 2 <= n <= 4095"

instance ToSample PwmValClock where
  toSamples _ = singleSample (PwmValClock 2)

instance FromHttpApiData PwmMde where
  parseUrlPiece "pwm_mode_bal" = pure (PwmMde PWM_MODE_BAL)
  parseUrlPiece "pwm_mode_ms"  = pure (PwmMde PWM_MODE_MS)
  parseUrlPiece bad            = Left bad
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "pwmMode" PwmMde) where
  toCapture _ =
    DocCapture "pwmMode"
               "(string) pwm_mode_bal || pwm_mode_ms"

instance ToSample PwmMde where
  toSamples _ =
    [ ("When the 'pwmMode' is pwm_mode_bal", PwmMde PWM_MODE_BAL)
    , ("When the 'pwmMode' is pwm_mode_ms", PwmMde PWM_MODE_MS) ]

instance FromHttpApiData DigByte where
  parseUrlPiece = fmap DigByte . parseUrlPiece
  parseQueryParam = parseUrlPiece

instance ToCapture (Capture "byte" DigByte) where
  toCapture _ =
    DocCapture "byte"
               "(word8) a byte"

instance ToSample DigByte where
  toSamples _ = singleSample (DigByte 128)

instance Accept PlainText => MimeRender PlainText Value where
  mimeRender _ LOW  = "low"
  mimeRender _ HIGH = "high"

instance ToSample Value where
  toSamples _ =
    [ ("When the value is low", LOW)
    , ("When the value is high", HIGH) ]

instance Accept PlainText => MimeRender PlainText Int where
  mimeRender _ n  = toByteString n

instance ToSample Int where
  toSamples _ = singleSample 2

data Health = OK deriving Show

instance Accept PlainText => MimeRender PlainText Health where
  mimeRender _ _  = "OK - PISERVER RUNNING"

instance ToSample Health where
  toSamples _ = singleSample OK

type PIAPI =
       "pinmode" :> "wpi" :> "1" :> "pwm_output" :> PostAccepted '[PlainText] NoContent
  :<|> "pinmode" :> "wpi" :> "7" :> "gpio_clock" :> PostAccepted '[PlainText] NoContent
  :<|> "pinmode" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Capture "digitalMode" DigitalMode :> PostAccepted '[PlainText] NoContent
  :<|> "pinmode" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Capture "digitalMode" DigitalMode :> PostAccepted '[PlainText] NoContent
  :<|> "pinmode" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Capture "digitalMode" DigitalMode :> PostAccepted '[PlainText] NoContent
  :<|> "pullupdncontrol" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Capture "pudValue" PudValue :> PostAccepted '[PlainText] NoContent
  :<|> "pullupdncontrol" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Capture "pudValue" PudValue :> PostAccepted '[PlainText] NoContent
  :<|> "pullupdncontrol" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Capture "pudValue" PudValue :> PostAccepted '[PlainText] NoContent
  :<|> "digitalread" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Get '[PlainText] Value
  :<|> "digitalread" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Get '[PlainText] Value
  :<|> "digitalread" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Get '[PlainText] Value
  :<|> "digitalwrite" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Capture "pinValue" PinValue :> PostAccepted '[PlainText] NoContent
  :<|> "digitalwrite" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Capture "pinValue" PinValue :> PostAccepted '[PlainText] NoContent
  :<|> "digitalwrite" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Capture "pinValue" PinValue :> PostAccepted '[PlainText] NoContent
  :<|> "pwmwrite" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Capture "pwmValue" PwmVal :> PostAccepted '[PlainText] NoContent
  :<|> "pwmwrite" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Capture "pwmValue" PwmVal :> PostAccepted '[PlainText] NoContent
  :<|> "pwmwrite" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Capture "pwmValue" PwmVal :> PostAccepted '[PlainText] NoContent
  :<|> "digitalwritebyte" :> Capture "byte" DigByte :> PostAccepted '[PlainText] NoContent
  :<|> "pwmsetmode" :> Capture "pwmMode" PwmMde :> PostAccepted '[PlainText] NoContent
  :<|> "pwmsetrange" :> Capture "pwmRange" PwmValRange :> PostAccepted '[PlainText] NoContent
  :<|> "pwmsetclock" :> Capture "pwmClock" PwmValClock :> PostAccepted '[PlainText] NoContent
  :<|> "piboardrev" :> Get '[PlainText] Int
  :<|> "pintobcmgpio" :> "wpi" :> Capture "wpiPinNumber" WpiPinNumber :> Get '[PlainText] Int
  :<|> "pintobcmgpio" :> "gpio" :> Capture "gpioPinNumber" GpioPinNumber :> Get '[PlainText] Int
  :<|> "pintobcmgpio" :> "phys" :> Capture "physPinNumber" PhysPinNumber :> Get '[PlainText] Int
  :<|> "health" :> Get '[PlainText] Health

piApi :: Proxy PIAPI
piApi = Proxy

type DOCSAPI = PIAPI :<|> Raw

api :: Proxy DOCSAPI
api = Proxy

docsPiApi :: API
docsPiApi = docs piApi

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] piApi
  where intro = DocIntro "Piserver" ["This is the Piserver's API", "Please read carefully!"]

