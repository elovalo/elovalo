# -*- mode: python; coding: utf-8 -*-
import subprocess

# http://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
# does not work on win!
def cmd_exists(cmd):
    return subprocess.call(["type", cmd],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

if cmd_exists('avr-gcc'):
    SConscript('avr.scons', variant_dir='build_avr', duplicate=0)

SConscript('exporter.scons', variant_dir='build_exporter', duplicate=0)
