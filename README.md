# piserv

A haskell-powered web server for Raspberry Pi GPIO control making use of the haskell [wiringPi](https://hackage.haskell.org/package/wiringPi) package and [servant](https://hackage.haskell.org/package/servant).

## Running the server

Download this repo and use the zipped binary `piserver` on your RPi. **Only tested on a Model B+ RPi as of yet**.

Please install the low-level [wiringpi](http://wiringpi.com/) library first on your RPi thus:

```
> sudo apt-get install wiringpi
```

Then run the binary on your RPi as either `root` or by setting the environment variable in your profile as follows:

```
> echo "export WIRINGPI_GPIOMEM=1" >> ~/.profile
```

The server will run by default on `PORT=8080`. You can parameterize the `PORT` for the executable at runtime as follows:

```
> ./piserver 8081 # where 8081 is port 8081 for example
```

You can test the server using a simple curl like:

```
curl -i http://localhost:8080/piboardrev
```

## Endpoints

Please see `API.md` for the automatically generated API documentation. You can get this by hitting `/docs` endpoint. Alternatively, any non-matched URL will return this documentation.

GET `/health` should return OK if the server is running.

## Building the package on your own RPi

I installed my GHC 8.0.1 / Stack LTS 7.18 / LLVM-3.7 setup using the [script](https://gist.github.com/jamesthompson/7730209b2b154bd0a182e6fe945a2838) here.

I include the binary as it took a long time to build. Some packages, notably `text`, `vector`, `bytestring`, `aeson`, `lens` are probably best installed one at a time using stack before unleashing the full `stack build`.

My RPi build (for binary included here) is as follows:

Linux raspberrypi 4.4.34-v7+ #930 armv7l GNU/Linux Raspbian "Jessie".


CPU Param | Value
----------|------
processor | 0
model name | ARMv7 Processor rev 5 (v7l)
BogoMIPS | 38.40
Features | half thumb fastmult vfp edsp neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm
CPU implementer | 0x41
CPU architecture | 7
CPU variant | 0x0
CPU part | 0xc07
CPU revision | 5

