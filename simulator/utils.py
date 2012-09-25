#
# Copyright 2012 Elovalo project group
#
# This file is part of Elovalo.
#
# Elovalo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Elovalo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
#
import argparse
import json
import os
from subprocess import call, Popen


ERROR = '\033[91m'
ENDC = '\033[0m'


def effect_parser():
    p = parser()
    p.add_argument('effect', help='name of the effect to render')
    p.add_argument('output', help='output path')

    return p


def parser():
    p = argparse.ArgumentParser()
    p.add_argument('--hd', help='render in HD', action='store_true')
    p.add_argument('--sensors', help='sensor data (.py) to use', type=str)

    return p


def export(effect, output, length=1.0, data='', sensors=''):
    """ Expects length in seconds!
    """
    length = os.environ.get('length', length) or 1.0
    length = float(length)
    length *= 1000  # convert to ms required by the exporter
    length = str(int(length))

    if not os.path.exists(output):
        os.mkdir(output)

    sensor_output = os.path.join(output, 'sensors.json')
    sensor_file = write_sensor_data(sensors, sensor_output, length)

    os.chdir('..')
    call('scons --no-avr', shell=True)
    os.chdir('simulator')
    d = ' ' + data if data else ''
    cmd = '../build/exporter/exporter ' + effect + ' ' + length + ' ' + \
            sensor_output + d
    print 'executing ' + cmd
    ret = call([cmd], shell=True)

    if ret:
        return write_fps(effect, output)

    error('Build failed!')


def write_sensor_data(sensors, output, length):
    data = parse_sensor_data(sensors, length)

    with open(output, 'w') as f:
        json.dump(data, f)


def parse_sensor_data(sensors, length):
    if not sensors:
        return

    length = int(length)
    sensors = sensors.replace('/', '.')

    try:
        # XXX: works only for pkg.module combo
        module = sensors.split('.')[1]
        pkg = __import__(sensors)
        mod = getattr(pkg, module)

        ret = {}
        for k, v in mod.sensors.items():
            ret[k] = [v(i) for i in range(length)]

        return ret
    except ImportError:
        error('Invalid sensor module!')


def write_fps(effect, output):
    with open(os.path.join(output, 'fps.json'), 'w') as f:
        p = os.path.join('exports', effect + '.json')

        if not os.path.exists(p):
            error('Missing export json. Check out your effect code!')
            return False

        with open(p, 'r') as jf:
            d = json.load(jf)

        json.dump(
            {
                'fps': d['fps']
            },
            f
        )

    return True


def error(msg):
    print ERROR + msg + ENDC


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
