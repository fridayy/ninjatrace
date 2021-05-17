# ninjatrace

_ninjatrace_ will be a live tracing system and drivers logbook for raspberrypis attached to a vehicles.

### Architecture

The architecture is based fully on distributed erlang/otp and everything else erlang and the BEAM provides out of the box.

A running ninjatrace application is either started as a *server* or a *device*. 

Devices are raspberry pis that are mounted on the motorcycle (or other vehicle) that should
be tracked by the system. A device generates data via multiple sensors (gps, thermal etc).
Devices connect to at least one well-known server using erlang distribution.

Servers are supposed to manage all connected devices and provide an API to access
the data produced by each device.

### RaspberryPI Settings

The executing user must have the rights to access the serial port `/dev/ttyS0`. Ideally by being a member 
of the `uucp` (or corresponding) group. 

`$ sudo gpasswd -a USER uucp`

**Quectel L80-M39 Settings**
The following stty serial settings must be provided for this device:
 `$ stty -F /dev/ttyS0 raw speed 9600`

### Build
    $ rebar3 compile

### Run a server

First pem containing a certificate and the private key must be generated:

```
$ openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes
$ cat cert.pem key.pem > merged.pem
```
