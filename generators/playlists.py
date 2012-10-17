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
import itertools
import subprocess
import yaml
import xml.etree.ElementTree as ET
from glob import glob

TICK_GRANULARITY = 0.008

def generate(source, target, conf, effects=None):
    parent_dir = os.path.split(target)[0]

    if not os.path.exists(parent_dir):
        os.mkdir(parent_dir)

    write(target, playlist_source(attach_ids(get_playlists(
        load(source), load_conf(conf)), get_names(effects))))


def load(source):
    ret = []

    for fmt, reader in (('json', json.loads), ('yaml', yaml.load),
            ('xml', load_xml)):
        for path, data in read(glob(os.path.join(source, '*.' + fmt)), reader):
            ret.append({
                'name': path.split('/')[-1].split('.')[0],
                'playlist': data,
            })

    return ret


def load_conf(path):
    if os.path.exists(path):
        with open(path, 'r') as f:
            return json.loads(f.read())


def get_playlists(data, conf):
    if conf:
        ret = []

        for name in conf:
            for d in data:
                if name == d['name']:
                    ret.append(d)
                    break

        return ret

    print 'Missing playlist conf! Using all playlists instead.'

    return data

def attach_ids(data, effects):
    for d in data:
        playlist = d['playlist']

        for effect in playlist:
            effect['id'] = str(effects.index(effect['name']))

    return data


def get_names(effects):
    return [e.split('/')[-1].split('.')[0] for e in effects]


def read(paths, reader):
    for path in paths:
        with open(path, 'r') as f:
            yield f.name, reader(f.read())


def write(path, data):
    with open(path, 'w') as t:
        t.write(data)


def playlist_source(data):
    file_start = '''/* GENERATED FILE! DON'T MODIFY!!! */
#include <stdint.h>
#include <stdlib.h>
#include "playlists.h"
#include "../pgmspace.h"
#include "../effects/lib/font8x8.h"
'''

    def custom_data(data):
        process = subprocess.Popen(["build/preprocessor/convert_glyphs"],
                                   stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE)

        def glyph_convert(s):
            sutf8 = s.encode('UTF-8')
            process.stdin.write(sutf8)
            process.stdin.write('\n')
            process.stdin.flush()
            return process.stdout.readline()

        def get_str(d):
            if d:
                return glyph_convert(d)

            return '{0}'

        effects = list(itertools.chain(*[d['playlist'] for d in data]))
        ret = ['PROGMEM const struct glyph_buf s_playlist_item_' + str(i) + ' = ' + \
            get_str(d.get('data', '')) + ';' for i, d in enumerate(effects)]

        process.terminate()

        return '\n'.join(ret) + '\n'

    def master_playlist(data):
        effects = list(itertools.chain(*[d['playlist'] for d in data]))
        effects_len = str(len(effects))
        ret = ['const playlistitem_t master_playlist[' + effects_len +
            '] PROGMEM = {']

        [ret.append('\t{ ' + str(fx['id']) + ', ' + str(int(fx['length']/TICK_GRANULARITY)) + \
            ', &s_playlist_item_' + str(i)  + ' },') for i, fx in enumerate(effects)]

        ret.append('};')

        ret.append('const uint8_t master_playlist_len = ' + effects_len + ';')

        return '\n'.join(ret) + '\n'

    def playlist_indices(data):
        playlists_len = str(len(data))
        ret = ['const uint8_t playlists[' + playlists_len +
            '] PROGMEM = {\n\t0,']

        fx_lengths = [len(d['playlist']) for d in data[:-1]]
        [ret.append('\t' + str(sum(fx_lengths[:i + 1])) + ',') for i in
                range(len(data) - 1)]

        ret.append('};')

        ret.append('const uint8_t playlists_len = ' + playlists_len + ';')

        return '\n'.join(ret) + '\n'

    return '\n'.join([
        file_start,
        custom_data(data),
        master_playlist(data),
        playlist_indices(data),
    ])


def load_xml(src):
    ret = []
    root = ET.fromstring(src)

    for effect in root.getchildren():
        data = effect.findall('data')
        ret.append({
            'name': effect.attrib['name'],
            'length': int(effect.findall('length')[0].text),
            'data': effect[0].text if len(data) else ''
        })

    return ret
