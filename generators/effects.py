import os
import re
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
        t.write('const uint8_t effects_len = sizeof(effects) / ' +
                'sizeof(effect_t);\n')
        t.write('\n')
        t.write(inp.functions)


class SourceFiles(object):

    def __init__(self, files):
        self._files = self._read(files)

    def _read(self, files):
        return [
            SourceFile(f) for f in files if SourceFile(f).name != 'template'
        ]

    @property
    def init_definitions(self):
        return self._definitions('init')

    @property
    def effect_definitions(self):
        return self._definitions('effect')

    def _definitions(self, k):
        init = lambda n: 'static void ' + k + '_' + n + '(void);'

        return '\n'.join([
                init(f.name) for f in self._files if getattr(f, k)
            ]) + '\n'

    @property
    def function_names(self):
        name = lambda n: 'PROGMEM const char s_' + n + '[] = "' + n + '";'

        return '\n'.join([name(f.name) for f in self._files]) + '\n'

    @property
    def effects(self):
        definition = lambda f: '\t{ s_' + f.name + ', ' + init(f) + ', ' + \
            effect(f) + ', 500, ' + flip(f) + ' },'
        init = lambda f: '&init_' + f.name if f.init else 'NULL'
        effect = lambda f: '&effect_' + f.name if f.effect else 'NULL'
        flip = lambda f: 'FLIP' if f.flip else 'NO_FLIP'

        ret = ['const effect_t effects[] PROGMEM = {']

        ret.extend([definition(f) for f in self._files])

        ret.append('};')

        return '\n'.join(ret) + '\n'

    @property
    def functions(self):
        # globals
        # utility funcs
        # init (rename)
        # effect (rename)
        return 'functions'  # TODO


class SourceFile(object):

    def __init__(self, path):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            content = analyze(f.readlines())

        self.globs = self._globals(content)
        self.functions = self._functions(content)
        self.init = self._init(content)
        self.effect = self._effect(content)
        self.flip = self._flip(content)

    def _globals(self, c):
        pass

    def _functions(self, c):
        pass

    def _init(self, c):
        # TODO: find init() block
        return len([True for line in c if 'init' in line['type']]) > 0

    def _effect(self, c):
        # TODO: find effect() block
        return len([True for line in c if 'effect' in line['type']]) > 0

    def _flip(self, c):
        return len([True for line in c if 'flip' in line['type']]) > 0


def analyze(content):
    def analyze(line):
        types = '(void|uint8_t|uint16_t|float|int)'
        patterns = (
            ('flip', '#\s*pragma\s+FLIP\s*'),
            ('init', 'void\s+init\s*[(]'),
            ('effect', 'void\s+effect\s*[(]'),
            ('function', '\s*' + types + '(\s+\w+\s*[(])'),
            ('assignment', '\s*' + types + '(\s+\w+)'),
        )
        t = [n for n, p in patterns if len(re.compile(p).findall(line)) > 0]

        return {
            'content': line,
            'type': t,
        }

    return [analyze(line) for line in content]
