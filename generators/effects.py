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
        t.write(inp.function_definitions)
        t.write(inp.function_names)
        t.write(inp.effects)
        t.write(inp.functions)


class SourceFiles(object):

    def __init__(self, files):
        self._files = self._read(files)

    def _read(self, files):
        return [SourceFile(f) for f in files]

    @property
    def function_definitions(self):
        return 'defs'  # TODO

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
        pass  # TODO: find init() block

    def _draw(self, c):
        pass  # TODO: find draw() block
