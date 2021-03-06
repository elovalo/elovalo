<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Elovalo LED Cube Pedestal

You should check out [elovalo.koodilehto.fi](http://elovalo.koodilehto.fi/) for more casual documentation. If you are interested in building the project and developing effects, read on.

## Dependencies

You should have at least gcc, avr-gcc, scons, gperf and preferably
jansson and Blender installed in order to use the simulator. If you
have Ubuntu 12.04 LTS or 12.10, we have instructions for you.

### Ubuntu 14.10

Something went wrong with GCC leading to this project not to compile. We are going to fix this after release of Ubuntu 15.04. If you happen te fix it, please send a patch!

### Ubuntu 12.10

To build on Ubuntu 12.10, install the following packages:

    sudo apt-get install gcc-avr avr-libc gcc scons libjansson-dev gperf avrdude

If you want to generate animations, install the following packages as well:

    sudo apt-get install blender libav-tools

### Ubuntu 12.04

With Ubuntu 12.04 LTS you need to install some packages from Quantal (12.10)
repositories. Here's how to do it:

Add the following lines to <tt>/etc/apt/preferences.d/10-quantal</tt>

    Package: *
    Pin: release a=quantal
    Pin-Priority: 100

And add the following line to <tt>/etc/apt/sources.list</tt>

    deb http://archive.ubuntu.com/ubuntu quantal main restricted universe multiverse

Now you can install all the required packages:

    sudo apt-get install gcc-avr/quantal avr-libc/quantal gcc scons libjansson-dev gperf avrdude

If you want to generate animations, install the following packages as well:

    sudo apt-get install blender libav-tools

The alternative to using quantal-pinning is to fetch the packages from
launchpad. This has only been verified on Kubuntu. You can also skip
the above steps and download the packages straight from the quantal
launchpad:
	
	https://launchpad.net/ubuntu/quantal/amd64/avr-libc/1:1.8.0-2
	https://launchpad.net/ubuntu/quantal/amd64/gcc-avr

If you have a 32bit system (tell 2002 we said *hi*), replace the amd64
from the URL to i386.

### OS X with SCons

Make sure you have at least gcc, avr-gcc and scons
installed. Preferably you should have jansson and Blender (2.6x) as
well if you wish to use the simulator.

In order to render with Blender, you'll need to define an alias
for. Just having it in your path isn't quite enough for some
reason. Add something like this to your .bashrc or .bash\_profile:

    alias blender="path to blender/blender.app/Contents/MacOS/blender"

### Eclipse 

You may use Eclipse for editing but you need SCons because of the complexity of this build (we are using code generation and multiple targets). It may be possible to use SCons in Eclipse if you feel adventurous:
http://stackoverflow.com/questions/1052759/eclipse-cdt-scons

## Building with SCons

The recommended way to build this project is SCons and we have
SCons build file for you. Just run:

    scons

Then the AVR output gets into `build/elo` and `build/zcl` and exporter
stuff to `build/exporter`. To run the exporter:

    build/exporter/exporter

If you want just to play with effets and you don't have an AVR compiler,
you may skip AVR build by running:

    scons --no-avr

## Programming AVR with SCons

If you are running Ubuntu 12.04 or newer and want to be able to flash without
root privileges, add the following udev rule to `/etc/udev/rules.d/10-usbasp`:

    SUBSYSTEM=="usb", ATTR{idVendor}=="16c0", ATTR{idProduct}=="05dc", MODE="0660", GROUP="adm"

Then just reboot your computer (or do some udev magic if you are Linux master).

To program Elocmd variant (the ordinary one), run:

    scons --program-elo
	
To program the variant with Zigbee support, run:

    scons --program-zcl

## Size Optimization

There is enough EEPROM but the SRAM usage is quite high because of
front and back buffers uses 0x300 bytes each. This leaves very scarce
resources for effect development. To see contents of SRAM, run:

    avr-objdump -t -j .data -j .bss build/zcl/release/firmware.elf

Bytes used in SRAM is printed every time scons is run. SRAM size in
ATmega328p is 2048 bytes. If SRAM gets too full, there is not enough
space for run-time stack. If so, you are in need of optimization.

If there are large arrays of constant data you should place the data
in program memory. See code in effects.c and main.c about effects[]
access. Also, read the following:

- http://nongnu.org/avr-libc/user-manual/pgmspace.html

- http://www.atmel.com/Images/doc8453.pdf

## Other

If you study the project further, you'll find more READMEs and more information related to specific parts. Have fun developing effects and whatnot!

## Credits

* [Joni Katajamäki](https://github.com/katis) - Embedded programming, hardware design 
* [Jukka Kinnunen](https://github.com/resutoor) - Hardware design and debugging
* [Joel Lehtonen](https://github.com/Zouppen/) - Embedded programming, effect engine programming
* [Lasse Saari](https://github.com/lassesaari) - Management, pedestal fabrication
* [Marko Silokunnas](https://github.com/marant) - Soldering
* [Mio Taalas](https://github.com/mtaalas/) - LED driver interface design, PCB design,
  embedded programming
* [Ilkka Turunen](https://github.com/ile2/) - Management, soldering, pedestal
  tests and aesthetic design
* [Juho Vepsäläinen](https://github.com/bebraw/) - 3D simulator, effect programming, pedestal
  design

## License

We use GPLv3 for software. Create Commons Attribution-ShareAlike is used for hardware and accompanying documentation. See LICENSE files for more details.

If you create a derivative, please let us know. It would be interesting to hear what you are doing.
