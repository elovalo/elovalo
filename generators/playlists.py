import os
import json
from glob import glob

file_start = '''/* GENERATED FILE! DON'T MODIFY!!! */
#include <stdint.h>
#include "../pgmspace.h"
#include "../playlist.h"
'''


def generate(source, target):
    for f in SourceFiles(glob(source)):
        c_path = os.path.join(target, f.name + '.c')
        h_path = os.path.join(target, f.name + '.h')

        with open(c_path, 'w') as t:
            t.write(f.c_lines)

        with open(h_path, 'w') as t:
            t.write(f.h_lines)


class SourceFiles(list):

    def __init__(self, files):
        super(SourceFiles, self).__init__(self._read(files))

    def _read(self, files):
        return [SourceFile(f) for f in files]


class SourceFile(object):

    def __init__(self, path):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            self._data = json.loads(f.read())

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
        definition = lambda f: '\t{ s_' + f['name'] + ', ' + str(f['length']) + ' },'

        ret = ['const playlistitem_t playlist[] PROGMEM = {']

        ret.extend([definition(f) for f in self._data])

        ret.append('\t{ NULL, 0},')
        ret.append('};')

        return '\n'.join(ret) + '\n'
