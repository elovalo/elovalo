<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Elovalo LED Cube Pedestal

You should check out [elovalo.org](http://elovalo.org/) for more casual documentation. If you are interested in building the project and developing effects, read on.

## Dependencies

You should have at least gcc, avr-gcc, scons, gperf and preferably jansson and Blender installed in order to use the simulator.

## Building

### Ubuntu with SCons

To build on Ubuntu install the following packages from the Quantal repository:

    avr-gcc gcc scons libjansson4

In order to install Quantal(12.10) packages you have to enable Quantal repositories:

Add the following lines to <tt>/etc/apt/preferences/</tt>

    Package: *
    Pin: release a=quantal
    Pin-Priority: 100

And add the following line to <tt>/etc/apt/sources.list</tt>

    deb http://archive.ubuntu.com/ubuntu quantal main restricted universe multiverse

Now you can install packages from the Quantal repository like this:

    sudo apt-get install avr-gcc/quantal avr-libc/quantal

You can compile the program by running

    # scons

If you want to generate animations, install the following packages as well:

    blender libav-tools

To be able to program the microcontroller, you'll need the programmer hardware and `avrdude` package.

### OS X with SCons

Make sure you have at least gcc, avr-gcc and scons installed. Preferably you should have jansson and Blender (2.6x) as well if you wish to use the simulator.

In order to render with Blender, you'll need to define an alias for. Just having it in your path isn't quite enough for some reason. Add something like this to your .bashrc or .bash\_profile:

alias blender="path to blender/blender.app/Contents/MacOS/blender"

### Eclipse 

*OUTDATED*

Import project to Eclipse using Projects from Git thingy.

Choose *Debug* (or some other) build configuration if you want to
build to AVR architecture.

To compile and run the PC exporter, qhoose *Exporter* as your build
configuration. You should run the produced executable (located at
<tt>Exporter</tt> subdirectory) in the same directory as this
file. For example, on Linux:

    Exporter/ledivilkku2.elf

## Building with SCons

If you feel yourself more comfortable at command prompt, there is
SCons build file for you. Just run:

    scons

Then the AVR output gets into `build_avr` and exporter stuff to
`build_exporter`. To run the exporter:

    build_exporter/exporter

If you want just to play with effets and you don't have an AVR compiler,
you may skip AVR build by running:

    scons --no-avr

## Programming AVR with SCons

If you are running Ubuntu 12.04 or newer and want to be able to flash without
root privileges, add the following udev rule to `/etc/udev/rules.d/10-usbasp`:

    SUBSYSTEM=="usb", ATTR{idVendor}=="16c0", ATTR{idProduct}=="05dc", MODE="0660", GROUP="adm"

Then just reboot your computer (or do some udev magic if you are Linux master).

To program, run:

    scons --program

## Size Optimization

There is enough EEPROM but the SRAM usage is quite high because of
front and back buffers uses 0x300 bytes each. This leaves very scarce
resources for effect development. To see contents of SRAM, run:

    avr-objdump -t -j .data -j .bss build/release/firmware.elf

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
