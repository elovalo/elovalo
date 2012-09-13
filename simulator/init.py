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
from subprocess import call


def parser():
    p = argparse.ArgumentParser()
    p.add_argument('effect')
    p.add_argument('path')
    p.add_argument('--hd', help='Render in HD', action='store_true')

    return p


def set_env(args):
    os.environ['effect'] = args.effect
    os.environ['path'] = args.path


def write_fps(effect, path):
    if not os.path.exists(path):
        os.mkdir(path)

    with open(os.path.join(path, 'fps.json'), 'w') as f:
        p = os.path.join('exports', effect + '.json')
        d = json.load(open(p, 'r'))
        json.dump(
            {
                'fps': d['fps']
            },
            f
        )


def execute(args):
    set_env(args)
    os.chdir('..')
    call('scons --no-avr', shell=True)
    os.chdir('simulator')
    length = os.environ.get('length', '100') or '100'
    call(['../build/exporter/exporter ' + length], shell=True)
    write_fps(args.effect, args.path)
