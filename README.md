ninjatrace
=====

_ninjatrace_ will be a live tracing system and drivers logbook of a GPS enhanced Raspberry Pi written in erlang. 
As this is mainly a (learning) playground for distributed erlang and OTP the architecture is far off from ideal and definitely "won't scale". :)

RaspberryPI
-----

The executing user must have the rights to access the serial port `/dev/ttyS0`. Ideally by being a member 
of the `uucp` (or corresponding) group. 

`$ sudo gpasswd -a USER uucp`

**Quectel L80-M39 Settings**
The following stty serial settings must be provided for this device:
 `$ stty -F /dev/ttyS0 raw speed 9600`

Build
-----

    $ rebar3 compile
