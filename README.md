# piserv

A haskell-powered web server for Raspberry Pi GPIO control making use of the haskell [wiringPi](https://hackage.haskell.org/package/wiringPi) package and [servant](https://hackage.haskell.org/package/servant).

Please install the low-level [wiringpi](http://wiringpi.com/) library first on your RPi thus:

```
> sudo apt-get install wiringpi
```


## Running the server

Download this repo and use the zipped binary `piserver` on your RPi.

Then run the binary on your RPi as either `root` or by setting the environment variable in your profile as follows:

```
> echo "export WIRINGPI_GPIOMEM=1" >> ~/.profile
```

The server will run by default on `PORT=8080`. You can parameterize it at runtime as follows:

```
> ./piserver 8081 #where 8081 is port 8081 for example
```

You can test the server using a simple curl like:

```
curl -i http://localhost:8080/piboardrev
```

