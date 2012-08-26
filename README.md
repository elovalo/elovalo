<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Elovalo LED Driver

Documentation TODO.

## Requirements

These requirements are for building with SCons in Debian GNU/Linux or Ubuntu. This should compile with Windows and Eclipse. TODO add instructions for Windows.

To build on Debian GNU/Linux or Ubuntu platform, install the following packages:

    avr-gcc gcc scons

If you want to generate animations, install the following packages as well:

    blender libav-tools

To be able to program the microcontroller, you'll need the programmer hardware and `avrdude` package.

## Building in Eclipse

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

## Size optimization

There is enough EEPROM but the SRAM usage is quite high because of
front and back buffers uses 0x300 bytes each. This leaves very scarce
resources for effect development. To see contents of SRAM, run:

    avr-objdump -t -j .data -j .bss build_release/ledcube.elf

Bytes used in SRAM is printed every time scons is run. SRAM size in
ATmega328p is 2048 bytes. If SRAM gets too full, there is not enough
space for run-time stack. If so, you are in need of optimization.

If there are large arrays of constant data you should place the data
in program memory. See code in effects.c and main.c about effects[]
access. Also, read the following:

- http://nongnu.org/avr-libc/user-manual/pgmspace.html

- http://www.atmel.com/Images/doc8453.pdf

## More information

For more information, feel free to contact Elovalo project group. You can visit the project website at http://hacklabjkl.org/projektit:living-light

## Generating animations

See file `simulator/README.md`.

## Credits

* [Joni Katajamäki](https://github.com/katis) - Embedded programming, hardware design 
* [Jukka Kinnunen](https://github.com/resutoor) - Hardware design and debugging
* [Joel Lehtonen](https://github.com/Zouppen/) - Embedded programming, effect engine programming
* Lasse Saari - Management, pedestal fabrication
* [Marko Silokunnas](https://github.com/marant) - Soldering
* [Mio Taalas](https://github.com/mtaalas/) - LED driver interface design, PCB design,
  embedded programming
* [Ilkka Turunen](https://github.com/ile2/) - Management, soldering, pedestal
  tests and aesthetic design
* [Juho Vepsäläinen](https://github.com/bebraw/) - 3D simulator, effect programming, pedestal
  design

## License

Coming soon. We are moving to GPLv3 for software and Creative Commons
Attribution-ShareAlike for hardware. More details are coming later. In
meantime, you must ask us before using or making derivate works based
on this.
