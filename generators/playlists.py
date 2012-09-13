import os
import json
import itertools
import yaml
import xml.etree.ElementTree as ET
from glob import glob


def generate(source, target, effects=None):
    write(target, playlist_source(attach_ids(load(source),
        get_names(effects))))


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
#include "playlist.h"
#include "pgmspace.h"
'''

    def master_playlist(data):
        effects = list(itertools.chain(*[d['playlist'] for d in data]))
        ret = ['const playlistitem_t master_playlist[' + str(len(effects)) +
            '] PROGMEM = {']

        [ret.append('\t{ ' + str(fx['id']) + ', ' + str(fx['length']) + ' },')
            for fx in effects]

        ret.append('};')

        return '\n'.join(ret) + '\n'

    def playlist_indices(data):
        ret = ['const uint8_t playlists[' + str(len(data)) +
            '] PROGMEM = {\n\t0,']

        fx_lengths = [len(d['playlist']) for d in data[:-1]]
        [ret.append('\t' + str(sum(fx_lengths[:i + 1])) + ',') for i in
                range(len(data) - 1)]

        ret.append('};')

        return '\n'.join(ret) + '\n'

    return '\n'.join([
        file_start,
        master_playlist(data),
        playlist_indices(data),
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
