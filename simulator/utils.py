import argparse
import json
import os
from subprocess import call, Popen


def effect_parser():
    p = parser()
    p.add_argument('effect', help='name of the effect to render')
    p.add_argument('output', help='output path')

    return p


def parser():
    p = argparse.ArgumentParser()
    p.add_argument('--hd', help='render in HD', action='store_true')

    return p


def export(effect, output, length=1.0):
    """ Expects length in seconds!
    """
    length = os.environ.get('length', length) or length
    length = float(length)
    length *= 1000  # convert to ms required by the exporter
    length = str(int(length))

    os.chdir('..')
    call('scons --no-avr', shell=True)
    os.chdir('simulator')
    call(['../build/exporter/exporter ' + effect + ' ' + length], shell=True)
    write_fps(effect, output)


def write_fps(effect, output):
    if not os.path.exists(output):
        os.mkdir(output)

    with open(os.path.join(output, 'fps.json'), 'w') as f:
        p = os.path.join('exports', effect + '.json')

        with open(p, 'r') as jf:
            d = json.load(jf)

        json.dump(
            {
                'fps': d['fps']
            },
            f
        )


def render_animation(effect, output, length):
    """ Expects length in seconds!
    """
    os.environ['effect'] = effect
    os.environ['output'] = output
    os.environ['length'] = str(length)

    sp = Popen(["/bin/bash", "-i", "-c",
        "blender -b blender/simulator.blend -P blender/sim.py -a"])
    sp.communicate()


def render_frame(effect, output, frame):
    os.environ['effect'] = effect
    os.environ['output'] = output

    sp = Popen(["/bin/bash", "-i", "-c",
        "blender -b blender/simulator.blend -P blender/sim.py -f " +
        frame]
    )
    sp.communicate()
