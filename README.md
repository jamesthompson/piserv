# piserv

A haskell-powered web server for Raspberry Pi GPIO control making use of the haskell [wiringPi](https://hackage.haskell.org/package/wiringPi) package and [servant](https://hackage.haskell.org/package/servant).

Please install the low-level [wiringpi](http://wiringpi.com/) library first on your RPi thus:

```
> sudo apt-get install wiringpi
```

Download this repo and use the zipped binary `piserver` on your RPi.

Then run the binary on your RPi as either `root` or by setting the environment variable in your profile as follows:

```
> echo "export WIRINGPI_GPIOMEM=1" >> ~/.profile
```

The server will run on `PORT=8080`.

You can test the server using a simple curl like:

```
curl -i http://localhost:8080/piboardrev
```

You can see the available routings in the [API](https://github.com/jamesthompson/piserv/blob/master/src/Piserv/API.hs#L98-L122) file.

