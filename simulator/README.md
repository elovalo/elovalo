# Effect Simulator

Requires Blender 2.62 or newer to run properly. Make sure you execute the
script in .blend (alt-p at the script window) before rendering out the
animation (bottom right button at 3D view).

## Rendering via terminal

It is possible to render via terminal as well. Make sure you have an alias for
Blender (OS X) or have it in your path (Ubuntu, Win?). Execute the following
command in order to render out an animation (a set of pngs) of given `effect`:

effect=sine path=tmp blender -b simulator.blend -P sim.py -a

This would render the sine effect in /tmp below the current path. You should
find a bunch of pngs there. It is possible to use certain other flags. Consider
the following:

* -f 25 - Renders frame 25 only
* -s 1 -e 50 -a - Starts rendering from frame 1 and ends at 50

## Converting pngs to something useful

If you have ffmpeg installed and available at the path, you can do something
like this:

ffmpeg -i tmp/%04d.png test.avi

This would convert pngs from tmp into a test.avi which you can then playback
using ffplay (ffplay test.avi) or some other app.

