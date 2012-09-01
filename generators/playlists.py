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
        target_path = os.path.join(target, f.name + '.c')

        with open(target_path, 'w') as t:
            t.write(f.lines)


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
    def lines(self):
        return '\n'.join([
            file_start,
            self.function_names,
            self.playlist
        ])

    @property
    def function_names(self):
        name = lambda n: 'PROGMEM const char s_' + n + '[] = "' + n + '";'

        return '\n'.join([name(f['name']) for f in self._data]) + '\n'

    @property
    def playlist(self):
        definition = lambda f: '\t{ s_' + f['name'] + ', ' + str(f['length']) + ' },'

        ret = ['const playlistitem_t playlist[] PROGMEM = {']

        ret.extend([definition(f) for f in self._data])

        ret.append('};')
        ret.append('const uint8_t playlist_len = sizeof(playlist) / ' +
                'sizeof(playlistitem_t);\n')

        return '\n'.join(ret) + '\n'
