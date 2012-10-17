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

import os
from SCons.Script import Glob, Environment
from SCons.Script.Main import GetOption

Elf = 'firmware.elf'


def source_files():
    "Return globbed list of sources. Add new source directories here if needed"
    return [Glob('src/*.c'),
        Glob('src/effects/lib/*.c'),
        Glob('src/generated/*.c'),
    ]


def avr_source_files():
    ret = source_files()
    ret.append(Glob('src/avr/*.c'))

    return ret


def exporter_source_files():
    ret = source_files()
    ret.append(Glob('src/exporter/*.c'))

    return ret


def avr_build_env(flags=''):
    """Returns environment suitable for AVR building. Flags should be
    compatible with both linker and compiler"""
    mmcu = '-mmcu=atmega328p'
    hz = '-DF_CPU=16000000UL'

    env = Environment(ENV=os.environ)
    env['CC'] = 'avr-gcc'
    env.Append(CCFLAGS=flags)
    env.Append(LINKFLAGS=flags)
    env.Append(CCFLAGS=mmcu + ' ' + hz + ' -Wall -std=gnu99 -fpack-struct ' +\
        '-fshort-enums -flto')
    env.Append(LINKFLAGS=mmcu + ' -flto -fwhole-program -flto-partition=none')
    env.Append(LIBS='m')
    if GetOption('use_asm'):
        env.Append(CPPDEFINES='ASM_ISRS')
    return env
