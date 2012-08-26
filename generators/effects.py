import os
from glob import glob

file_start = '''/* GENERATED FILE! DON'T MODIFY!!!
 * Led cube effects
 */

#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include "pgmspace.h"
#include "env.h"
#include "effects.h"
#include "effects/lib/utils.h"
#include "effects/lib/text.h"
'''


def generate(source, target):
    inp = SourceFiles(glob(source + '/*.c'))

    with open(target, 'w') as t:
        t.write(file_start)
        t.write('\n')
        t.write(inp.init_definitions)
        t.write('\n')
        t.write(inp.effect_definitions)
        t.write('\n')
        t.write(inp.function_names)
        t.write('\n')
        t.write(inp.effects)
        t.write('\n')
        t.write(inp.functions)


class SourceFiles(object):

    def __init__(self, files):
        self._files = self._read(files)

    def _read(self, files):
        return [SourceFile(f) for f in files]

    @property
    def init_definitions(self):
        init = lambda n: 'static void init_' + n + '(void);'

        return '\n'.join([init(f.name) for f in self._files if f.init])

    @property
    def effect_definitions(self):
        return 'effects'  # TODO

    @property
    def function_names(self):
        name = lambda n: 'PROGMEM const char s_' + n + '[] = "' + n + '";'

        return '\n'.join([name(f.name) for f in self._files])

    @property
    def effects(self):
        return 'effects'  # TODO

    @property
    def functions(self):
        return 'functions'  # TODO


class SourceFile(object):

    def __init__(self, path):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            content = f.readlines()

        self.init = self._init(content)
        self.draw = self._draw(content)

    def _init(self, c):
        # TODO: find init() block
        return len([True for line in c if line.find('void init(') >= 0]) > 0

    def _draw(self, c):
        pass  # TODO: find draw() block
