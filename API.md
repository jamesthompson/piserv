## Piserver

This is the Piserver's API

Please read carefully!

## GET /digitalread/gpio/:gpioPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- When the value is low

```
low
```

- When the value is high

```
high
```

## GET /digitalread/phys/:physPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- When the value is low

```
low
```

- When the value is high

```
high
```

## GET /digitalread/wpi/:wpiPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- When the value is low

```
low
```

- When the value is high

```
high
```

## POST /digitalwrite/gpio/:gpioPinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwrite/phys/:physPinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwrite/wpi/:wpiPinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwritebyte/:byte

#### Authentication



Clients must supply the following data


#### Captures:

- *byte*: (word8) a byte

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## GET /health

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
OK - PISERVER RUNNING
```

## GET /piboardrev

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## POST /pinmode/gpio/:gpioPinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31
- *digitalMode*: (string) input || output

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pinmode/phys/:physPinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54
- *digitalMode*: (string) input || output

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pinmode/wpi/1/pwm_output

#### Authentication



Clients must supply the following data


#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pinmode/wpi/7/gpio_clock

#### Authentication



Clients must supply the following data


#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pinmode/wpi/:wpiPinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31
- *digitalMode*: (string) input || output

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## GET /pintobcmgpio/gpio/:gpioPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## GET /pintobcmgpio/phys/:physPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## GET /pintobcmgpio/wpi/:wpiPinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## POST /pullupdncontrol/gpio/:gpioPinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31
- *pudValue*: (string) pud_off || pud_up || pud_down

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pullupdncontrol/phys/:physPinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54
- *pudValue*: (string) pud_off || pud_up || pud_down

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pullupdncontrol/wpi/:wpiPinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31
- *pudValue*: (string) pud_off || pud_up || pud_down

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmsetclock/:pwmClock

#### Authentication



Clients must supply the following data


#### Captures:

- *pwmClock*: (int) 2 <= n <= 4095

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmsetmode/:pwmMode

#### Authentication



Clients must supply the following data


#### Captures:

- *pwmMode*: (string) pwm_mode_bal || pwm_mode_ms

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmsetrange/:pwmRange

#### Authentication



Clients must supply the following data


#### Captures:

- *pwmRange*: (int) 0 <= n <= 4096

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmwrite/gpio/:gpioPinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *gpioPinNumber*: (int) 0 <= n <= 31
- *pwmValue*: (int) default range = 0 <= n <= 1024, but range can go to 4096 by calling pwmsetrange

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmwrite/phys/:physPinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *physPinNumber*: (int) 3, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 31, 32, 33, 35, 36, 37, 38, 40, 51, 52, 53, 54
- *pwmValue*: (int) default range = 0 <= n <= 1024, but range can go to 4096 by calling pwmsetrange

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmwrite/wpi/:wpiPinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *wpiPinNumber*: (int) 0 <= n <= 31
- *pwmValue*: (int) default range = 0 <= n <= 1024, but range can go to 4096 by calling pwmsetrange

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

