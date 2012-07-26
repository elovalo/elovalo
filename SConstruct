# -*- mode: python; coding: utf-8 -*-

AddOption('--no-avr',
          dest='build_avr',
          action='store_false',
          default=True,
          help='Do not build to AVR architecture')

AddOption('--no-exporter',
          dest='build_exporter',
          action='store_false',
          default=True,
          help='Do not build exporter')

if GetOption('build_avr'):
    SConscript('avr.scons', variant_dir='build_avr', duplicate=0)

if GetOption('build_exporter'):
    SConscript('exporter.scons', variant_dir='build_exporter', duplicate=0)
