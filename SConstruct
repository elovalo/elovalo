# -*- mode: python; coding: utf-8 -*-

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

if GetOption('build_avr'):
    SConscript('debug.scons', variant_dir='build_debug', duplicate=0)

if GetOption('build_avr') and GetOption('build_release'):
    SConscript('release.scons', variant_dir='build_release', duplicate=0)

if GetOption('build_exporter'):
    SConscript('exporter.scons', variant_dir='build_exporter', duplicate=0)
