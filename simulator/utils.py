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
