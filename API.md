## Piserver

This is the Piserver's API

Please read carefully!

## GET /digitalread/gpio/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

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

## GET /digitalread/phys/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

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

## GET /digitalread/wpi/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

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

## POST /digitalwrite/gpio/:pinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwrite/phys/:pinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwrite/wpi/:pinNumber/:pinValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pinValue*: (string) low || high

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /digitalwritebytes/:byte

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

## POST /pinmode/gpio/:pinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *digitalMode*: (string) input || output

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pinmode/phys/:pinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
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

## POST /pinmode/wpi/:pinNumber/:digitalMode

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *digitalMode*: (string) input || output

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## GET /pintobcmgpio/gpio/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## GET /pintobcmgpio/phys/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## GET /pintobcmgpio/wpi/:pinNumber

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```
2
```

## POST /pullupdncontrol/gpio/:pinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pudValue*: (string) pud_off || pud_up || pud_down

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pullupdncontrol/phys/:pinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pudValue*: (string) pud_off || pud_up || pud_down

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pullupdncontrol/wpi/:pinNumber/:pudValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
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

## POST /pwmwrite/gpio/:pinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pwmValue*: (int)

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmwrite/phys/:pinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pwmValue*: (int)

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

## POST /pwmwrite/wpi/:pinNumber/:pwmValue

#### Authentication



Clients must supply the following data


#### Captures:

- *pinNumber*: (int) 0 <= n < 27
- *pwmValue*: (int)

#### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- Response body as below.

```

```

