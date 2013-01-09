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

./render sine tmp

This will render the sine effect in /tmp directory below the current path.

You can stop the execution at any time by hitting ctrl-c. After doing this you
should be able to find a bunch of pngs at the target directory. Note that it is
possible to use --length option to define explicitly how many seconds of
animation you want. Use either -h or --help flags to see all available options.

### Rendering Individual Frames

If you want to render just one frame, use

./render sine tmp 25

where 25 is the frame number you want.

Note that you can pass --hd flag to either command. This make it output frames
in HD (1080p) quality.

## Converting pngs to Something Useful

If you have ffmpeg installed and available at your path, execute

./toavi tmp test

This will convert pngs from tmp into a test.avi which you can then playback
using ffplay (ffplay test.avi) or some other app.

## Passing Sensor Data

If your effect happens to use sensor data, it can be handy to pass mock it.
This is done by passing a Python file containing mock generators to a renderer.

./render sine tmp 25 --sensors sensors/data

In this case "data" is a Python module residing in the sensors package. It is
important this file contains a definition along this:

```python
def constant(i):
    return lambda a: i

sensors = {
    "distance1": constant(1),
    "distance2": constant(10),
    "ambient_light": constant(5),
    "sound_pressure": constant(8),
}
```

As you can see sensors is a dictionary containing data definitions. Each key
value pair has some sensor name and associated mock data generator. In this
case the generators produce just constant values. You can easily pass something
more random instead.
