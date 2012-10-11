# -*- mode: python; coding: utf-8 -*-
import os
from glob import glob

from generators import effects, playlists

cwd = GetLaunchDir()

effects.generate(
    os.path.join(cwd, 'src', 'effects') + '/*.c',
    os.path.join(cwd, 'src', 'effects.c')
)

playlists.generate(
    os.path.join(cwd, 'src/playlists/'),
    os.path.join(cwd, 'src', 'playlists.c'),
    os.path.join(cwd, 'src', 'playlists.json'),
    effects=glob(os.path.join(cwd, 'src', 'effects') + '/*.c')
)

AddOption('--no-avr',
          dest='build_avr',
          action='store_false',
          default=True,
          help='Do not build to AVR architecture')

AddOption('--program-zcl',
          dest='program-zcl',
          action='store_true',
          default=False,
          help='Program the ZCL variant to microcontroller after build')

AddOption('--program-elo',
          dest='program-elo',
          action='store_true',
          default=False,
          help='Program the ELO variant to microcontroller after build')

AddOption('--no-exporter',
          dest='build_exporter',
          action='store_false',
          default=True,
          help='Do not build exporter')

AddOption('--no-asm',
          dest='use_asm',
          action='store_false',
          default=True,
          help='Do not use optimized interrupt handlers')

def build_avr(build_type):
    Export('build_type')
    SConscript('debug.scons', duplicate=0,
               variant_dir='build/'+build_type+'/debug')
    SConscript('release.scons', duplicate=0,
               variant_dir='build/'+build_type+'/release')

if GetOption('build_avr'):
    SConscript('simulation.scons', variant_dir='build/simulation', duplicate=0)
    build_avr('zcl')
    build_avr('elo')

if GetOption('build_exporter'):
    SConscript('exporter.scons', variant_dir='build/exporter', duplicate=0)
