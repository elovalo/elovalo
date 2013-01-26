# Developing Effects

There are a few things you should know about developing effects. Usually the
easiest way to get started is just to copy template.c. That will give you a
nice base to build your effect upon. By examining the file, you can see there
are a few conventions you should follow.

## Structure

Each effect is structured rougly similarly:

```c
# pragma FLIP /* flip if defined. remove if you don't want to flip buffers */

#include "common.h" /* imports plenty of good stuff */

/* optional init run once at start */
void init(void)
{

}

/* optional effect code run perpetually after init */
void effect(void)
{

}
```

As you can see, there's a bit of configuration (to flip or not), include as you
might expect and then some functionality in the form of init and effect
definitions. It is important to stick to this naming scheme.

Note that you do not have to define a header file corresponding to the source
unlike typically. This is due to the fact that the effect code written will
get transpiled into another c file that contains each of the effects.

You can inspect this file (effects.c), once generated, at the root of /src. It
should more or less correspond with the effect contents and contain some
additional data and definitions needed to make the code ideal for AVR.

## Double Buffering

In the beginning you can define whether or not the effect will flip using
"# pragma FLIP". If this has not been defined, the effect will not flip buffers.
This can be handy for accumulating visible leds.

In case double buffering (flip on) is used, you may expect the code to perform
faster. In this case you cannot rely on the fact that the old buffer contains
valid data, though.

## init

It can be handy to set up various initial states at the init. If you wish to
mutate some state, define a global. You can refer to it at the effect later.

Usually the use of globals is a big no-no. Given we are dealing with a somewhat
limited target, it is just about acceptable here.

## effect

Unless you are dealing with an entirely static effect which you set up at init,
you will most likely end up defining some effect code. Perform the math you
want here.

As writing in vanilla C can be somewhat boring and arduous at times, we have
developed specific kernel macros that make it easier to define effects.
Currently there are two of these: XY and XYZ.

### XY

XY is a kernel that iterates each xy pair.

### XYZ

XYZ is a kernel that gives access to each xyz triad on the coordinate
system. As XY, this kernel does not expect a return value. Instead you
can set intensity of the led you want.

Note that both kernels clear buffer before iterating.

## Coordinate System

The origin of the cube has been set on top-left corner (front view) just like
for regular displays. x increments towards right in this case while z towards
bottom. The remaining y axis increments towards depth.

If you want to understand how this works in a visual way, write an effect that
sets the origin visible and render that through the simulator.

## Utilities

Besides XY and XYZ, there are certain functions you may find useful.
There is a simple text API, shape library and voxel manipulation utilities.

### Text

The text API contains two simple functions: scroll_text and render_character.
As you might guess, the latter allows you to render a single character on the
display. Given its low resolution, it allows you to display only eight at a
time. Usually you want to show just one to keep it legible.

To make the letter "w" appear on the display, you could try something like
"render_character('w', 0, MAX_INTENSITY, 0)". The first parameter defines
the character, the second one is used to define its location in depth,
third for intensity and the last for horizontal offset.

"scroll_text" takes things further and allows you to display text while
scrolling. In this case you will pass the text snippet you want as the first
parameter. Otherwise the call is the same.

### Shapes

If you want to use some predefined shape or define your own, it's preferable
to do it in the shape library (lib/shapes.c). Currently there are only few
available but that might be enough to get started. Examine the effects using
them to get a better idea on how to use them.

### Voxel Manipulation

In the end of the day it all comes down to voxel manipulation. You will want to
set leds on and off or to some specific intensity. For this purpose there are
various functions. "set_led" is perhaps the most useful one of these. Just
provide xyz triad and an intensity to it and off you go.

You may also find it handy to be able to set entire row (set_row) in depth. In
this case something like "set_row(0, 0, 2, 7, intensity)" will work. The first
parameter sets the row direction in depth, the second vertically. 2 and 7
define the span horizontally.

## Tips and Tricks

1. There isn't a lot of memory available. Use existing data (ie. buffers) to
your advantage.
2. Examine the existing effects to see how they do their thing.
3. Read through /lib to see what kind of utilities are available.
4. If the API is missing something, let us know or implement it yourself and
drop a pull request.

## Debugging with exporter

You may find exporter handy for testing and debugging the
effects. Exported effects may be displayed in hardware via elocmd in
binary format or rendered using Blender in json format.

If your effect is not working correctly you may want to debug them using real debugger. Its quite easy because exporter is running on your computer, not in the MCU. Just use the ordinary debugging tools.

Here is an example using gdb. You may use a graphical debugger, too. Remember to 

    $ ulimit -c unlimited
    $ ./build/exporter/exporter -b scroll_text 10 "" kaatuu
    Exporting 10.000000 seconds of scroll_text to file exports/scroll_text.elo
    Segmentation fault (core dumped)

Then run gdb:

    $ gdb build/exporter/exporter core 
    GNU gdb (Ubuntu/Linaro 7.4-2012.04-0ubuntu2.1) 7.4-2012.04
    ...

Then run backtrace, for example:

    (gdb) backtrace 
    #0  render_character (glyph_p=0x544e4547415f4853, offset=9, f=0x401c20 <render_xy>) at /usr/include/x86_64-linux-gnu/bits/string3.h:52
    #1  0x0000000000403531 in effect_scroll_text () at src/effects/scroll_text.c:53
    #2  0x0000000000403c98 in export_effect (effect=0x404fc0, length=10, sensor_path=<optimised out>, data=<optimised out>, binary=true) at src/exporter/exporter.c:164
    #3  0x0000000000400c0b in main (argc=<optimised out>, argv=0x7fff478e2dc0) at src/exporter/exporter.c:68

See more at http://www.network-theory.co.uk/articles/gccdebug.html
