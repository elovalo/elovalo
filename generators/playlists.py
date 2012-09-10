import os
import json
import yaml
import xml.etree.ElementTree as ET
from glob import glob

file_start = '''/* GENERATED FILE! DON'T MODIFY!!! */
#include <stdint.h>
#include "../pgmspace.h"
#include "../playlist.h"
'''


def generate(source, target):
    for fmt, reader in (('json', json.loads), ('yaml', yaml.load),
            ('xml', load_xml)):
        for f in SourceFiles(glob(os.path.join(source, '*.' + fmt)), reader):
            c_path = os.path.join(target, f.name + '.c')
            h_path = os.path.join(target, f.name + '.h')

            with open(c_path, 'w') as t:
                t.write(f.c_lines)

            with open(h_path, 'w') as t:
                t.write(f.h_lines)


def load_xml(src):
    ret = []
    root = ET.fromstring(src)

    for effect in root.getchildren():
        ret.append({
            "name": effect.attrib["name"],
            "length": effect.findall('length')[0].text
        })

    return ret


class SourceFiles(list):

    def __init__(self, files, reader):
        super(SourceFiles, self).__init__(self._read(files, reader))

    def _read(self, files, fmt):
        return [SourceFile(f, fmt) for f in files]


class SourceFile(object):

    def __init__(self, path, reader):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            self._data = reader(f.read())

    @property
    def c_lines(self):
        return '\n'.join([
            file_start,
            self.function_names,
            self.playlist
        ])

    @property
    def h_lines(self):
        return '\n'.join([
            "/* GENERATED FILE! DON'T MODIFY!!! */",
            '#include "../playlist.h"',
            '\n'
            'extern const playlistitem_t playlist[];',
        ])

    @property
    def function_names(self):
        names = lambda d: set([f['name'] for f in d])
        name = lambda n: 'PROGMEM const char s_' + n + '[] = "' + n + '";'

        return '\n'.join([name(n) for n in names(self._data)]) + '\n'

    @property
    def playlist(self):
        definition = lambda f: '\t{ s_' + f['name'] + ', ' + \
            str(f['length']) + ' },'

        ret = ['const playlistitem_t playlist[] PROGMEM = {']

        ret.extend([definition(f) for f in self._data])

        ret.append('\t{ NULL, 0},')
        ret.append('};')

        return '\n'.join(ret) + '\n'
