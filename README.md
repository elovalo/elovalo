<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Led cube thingy

Documentation TODO.

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

## More information

For more information, feel free to contact Elovalo project group.
