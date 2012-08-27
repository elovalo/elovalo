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
        merge = lambda f: f.globs + f.functions + f.init + f.effect

        return '\n'.join(merge(f) for f in self._files) + '\n'


class SourceFile(object):

    def __init__(self, path):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            content = analyze(f.readlines())

        self.globs = self._globals(content)
        self.functions = self._functions(content)
        self.init = self._block(content, 'init')
        self.effect = self._block(content, 'effect')
        self.flip = self._block(content, 'flip')

    def _globals(self, c):
        return ''

    def _functions(self, c):
        return ''

    def _block(self, c, name):
        #  TODO: init, effect prefix
        def parse(index):
            ret = definition(c[index])
            cc = c[index]['content']

            if cc.startswith('#'):
                return ret

            begin_braces = cc.count('{')
            end_braces = 0

            offset = 0
            if not begin_braces:
                begin_braces, end_braces, offset = find_begin_braces(index + 1)

            for i in range(index + 1 + offset, len(c)):
                cc = c[i]['content']
                ret += cc
                begin_braces += cc.count('{')
                end_braces += cc.count('}')

                if begin_braces == end_braces:
                    return ret

            return ret

        def definition(c):
            if 'init' in c['types']:
                return 'static void init_' + self.name + '(void)'

            if 'effect' in c['types']:
                return 'void effect_' + self.name + '(void)'

            return c['content']

        def find_begin_braces(i):
            for j in range(i, len(c)):
                b = c.count('{')

                if b:
                    return b, c.count('}'), j - i

            return 0, 0, 0

        return ''.join(parse(i) for i, line in enumerate(c)
            if name in line['types'])


def analyze(content):
    def analyze(i, line):
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
            'types': t,
            'index': i,
        }

    return [analyze(i, line) for i, line in enumerate(content)]
