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
    os.path.join(cwd, 'src/playlists/'),
    effects=glob(os.path.join(cwd, 'src', 'effects') + '/*.c')
)

AddOption('--no-avr',
          dest='build_avr',
          action='store_false',
          default=True,
          help='Do not build to AVR architecture')

AddOption('--no-release',
          dest='build_release',
          action='store_false',
          default=True,
          help='Do not build release target')

AddOption('--program',
          dest='program',
          action='store_true',
          default=False,
          help='Program the microcontroller after build')

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

if GetOption('build_avr'):
    SConscript('debug.scons', variant_dir='build/debug', duplicate=0)

if GetOption('build_avr') and GetOption('build_release'):
    SConscript('release.scons', variant_dir='build/release', duplicate=0)

if GetOption('build_exporter'):
    SConscript('exporter.scons', variant_dir='build/exporter', duplicate=0)
