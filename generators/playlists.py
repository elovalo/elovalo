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
import json
import yaml
import xml.etree.ElementTree as ET
from glob import glob


def generate(source, target):
    for data in load(source):
        write(os.path.join(target, data['name'] + '.c'),
                c_source(data['playlist']))
        write(os.path.join(target, data['name'] + '.h'),
                h_source(data['playlist']))


def load(source):
    ret = []

    for fmt, reader in (('json', json.loads), ('yaml', yaml.load),
            ('xml', load_xml)):
        for path, data in read(glob(os.path.join(source, '*.' + fmt)), reader):
            ret.append({
                'name': path.split('/')[-1].split('.')[0],
                'playlist': data
            })

    return ret


def read(paths, reader):
    for path in paths:
        with open(path, 'r') as f:
            yield f.name, reader(f.read())


def write(path, data):
    with open(path, 'w') as t:
        t.write(data)


def c_source(data):
    file_start = '''/* GENERATED FILE! DON'T MODIFY!!! */
#include <stdint.h>
#include "../pgmspace.h"
#include "../playlist.h"
'''

    def function_names(data):
        names = lambda d: set([f['name'] for f in d])
        name = lambda n: 'PROGMEM const char s_' + n + '[] = "' + n + '";'

        return '\n'.join([name(n) for n in names(data)]) + '\n'

    def playlist(data):
        definition = lambda f: '\t{ s_' + f['name'] + ', ' + \
            str(f['length']) + ' },'

        ret = ['const playlistitem_t playlist[] PROGMEM = {']

        ret.extend([definition(f) for f in data])

        ret.append('\t{ NULL, 0},')
        ret.append('};')

        return '\n'.join(ret) + '\n'

    return '\n'.join([
        file_start,
        function_names(data),
        playlist(data)
    ])


def h_source(data):
    return '\n'.join([
            "/* GENERATED FILE! DON'T MODIFY!!! */",
            '#include "../playlist.h"',
            '\n'
            'extern const playlistitem_t playlist[];',
        ])


def load_xml(src):
    ret = []
    root = ET.fromstring(src)

    for effect in root.getchildren():
        ret.append({
            "name": effect.attrib["name"],
            "length": effect.findall('length')[0].text
        })

    return ret
