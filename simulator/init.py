import argparse
import os
from subprocess import call


def parser():
    p = argparse.ArgumentParser()
    p.add_argument('effect')
    p.add_argument('path')
    p.add_argument('--hd', help="Render in HD", action='store_true')
    p.add_argument('--fps', help="Framerate", metavar="FPS", type=int)

    return p


def set_env(args):
    os.environ['effect'] = args.effect
    os.environ['path'] = args.path

    if args.hd:
        os.environ['hd'] = '1'

    if args.fps:
        os.environ['fps'] = str(args.fps)


def execute(args):
    set_env(args)
    os.chdir('..')
    call('scons --no-avr', shell=True)
    os.chdir('simulator')
    call('../build_exporter/exporter', shell=True)
