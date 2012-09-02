# Effect Simulator

The effect simulator requires Blender 2.62 or newer to run properly. Make sure
you have also project dependencies (gcc, avr-gcc, scons) installed before
proceeding. It is also vital that Blender is available on your path (Linux,
Windows) or that you have created an alias for it (OS X).

The simulator has been designed to be used via terminal. Note that those
proficient with Blender may find it useful for development as well. In this
case you should check out simulator.blend. It contains a script that loads
animation data provided by our exporter.

## Design

Our system makes it possible for you to write some C code that will run both on
desktop and the target device (AVR). While its run on desktop, it performs
various assertations. This way you may catch certain errors and avoid crashing
our puny AVR.

There are two scripts that make it possible to obtain preview data. It is
possible to render either a specific frame or a whole animation. The animation
will be available as individual frames that can then be composed into a video
using a specific utility such as ffmpeg.

The scripts work roughly in the following way:

1. Compile the code using scons
2. Export animation data for each effect. The overhead is so small we haven't
bothered to export it some specific effect.
3. Render using Blender

## Rendering via Terminal

To make it render perpetually, execute

./anim sine tmp

This will render the sine effect in /tmp directory below the current path.

You can stop the execution at any time by hitting ctrl-c. After doing this you
should be able to find a bunch of pngs at the target directory. Note that it is
possible to use --length option to define explicitly how many seconds of
animation you want. Use either -h or --help flags to see all available options.

### Rendering Individual Frames

If you want to render just one frame, use

./frame sine tmp 25

where 25 is the frame number you want.

Note that you can pass --hd flag to either command. This make it output frames
in HD (1080p) quality.

## Converting pngs to something useful

If you have ffmpeg installed and available at your path, execute

./toavi tmp test

This will convert pngs from tmp into a test.avi which you can then playback
using ffplay (ffplay test.avi) or some other app.

# Developing Effects

Keep the following pointers in mind while developing new effects:

1. Read src/effects.c, src/effects.h, src/effect\_utils.c and
src/effect\_utils.h carefully.
2. When adding new effects, make sure it ends up as last in the effect list.
This way effect indices used at serial communication won't get borked.
3. Learn to use ./anim and ./frame effectively.
4. If you think the API is missing something, either implement the missing bit
yourself or add a note about it to the issue tracker.

