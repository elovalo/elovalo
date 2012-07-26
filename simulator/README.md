# Effect Simulator

Requires Blender 2.62 or newer to run properly. Make sure you execute the
script in .blend (alt-p at the script window) before rendering out the
animation (bottom right button at 3D view).

## Rendering via terminal

It is possible to render via terminal as well. Make sure you have an alias for
Blender (OS X) or have it in your path (Ubuntu, Win?). Execute

./anim sine tmp

This would render the sine effect in /tmp below the current path. You should
find a bunch of pngs there. If you want to render just one frame, use

./frame sine tmp 25

where 25 is the frame number you want.

## Converting pngs to something useful

If you have ffmpeg installed and available at your path, execute

./toavi tmp test

This will convert pngs from tmp into a test.avi which you can then playback
using ffplay (ffplay test.avi) or some other app.

