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

For more information, feel free to contact Elovalo project group.
