# Led cube thingy

Documentation TODO.

## Building on AVR

Import project to Eclipse and choose *Debug* (or some other) build
configuration.

TODO More coming later.

## Building and running JSON exporter

To compile and run the PC exporter, either run the following on command line:

    gcc -Wall --std=gnu99 -o exporter effects.c exporter.c -lm
	./exporter

or import the project to Eclipse and choose *Exporter* as your build
configuratin. You should run the produced executable (located at
<tt>Exporter</tt> subdirectory) in the same directory as this
file. For example, on Linux:

    Exporter/ledivilkku2.elf

For more information, feel free to contact Elovalo project group.
