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
#include "effects/lib/shapes.h"
'''


def generate(source, target):
    inp = SourceFiles(glob(source))

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
        return [SourceFile(f) for f in files]

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
            effect(f) + ', ' + flip(f) + ' },'
        init = lambda f: '&init_' + f.name if f.init else 'NULL'
        effect = lambda f: '&effect_' + f.name if f.effect else 'NULL'
        flip = lambda f: 'FLIP' if f.flip else 'NO_FLIP'

        ret = ['const effect_t effects[] PROGMEM = {']

        ret.extend([definition(f) for f in self._files])

        ret.append('};')

        return '\n'.join(ret) + '\n'

    @property
    def functions(self):
        merge = lambda f: f.globs + f.function_declarations + f.functions + \
            f.init + f.effect

        return '\n'.join(merge(f) for f in self._files) + '\n'


class SourceFile(object):

    def __init__(self, path):
        self.name = os.path.splitext(os.path.basename(path))[0]

        with open(path, 'r') as f:
            content = analyze(self.name, f.readlines())

        self.globs = self._globals(content)
        self.function_declarations = self._function_declarations(content)
        self.functions = self._functions(content)
        self.init = self._block(content, 'init')
        self.effect = self._block(content, 'effect')
        self.flip = self._block(content, 'flip')

    def _globals(self, c):
        return '\n'.join(find_globals(c))

    def _function_declarations(self, c):
        return ''.join(line['content'] for line in c
            if 'function_declaration' in line['types']
        )

    def _functions(self, c):
        return ''.join(line['block'] for line in c
            if 'function' in line['types'] and len(line['types']) == 1
        )

    def _block(self, c, name):
        return ''.join(line['block'] for line in c if name in line['types'])


def find_globals(content):
    ret = []

    i = 0
    while i < len(content):
        c = content[i]

        if 'assignment' in c['types']:
            ret.append(c['content'])
        if 'function' in c['types']:
            i += len(c['block'].split('\n'))
        else:
            i += 1

    return ret


def analyze(name, content):
    def analyze_line(i, line):
        types = '(void|uint8_t|uint16_t|float|int|char|double)'
        patterns = (
            ('flip', '#\s*pragma\s+FLIP\s*'),
            ('init', 'void\s+init\s*[(]'),
            ('effect', '\s*effect\s*'),
            ('function', '\s*' + types + '(\s+\w+\s*[(])'),
            ('assignment', '\s*' + types + '(\s+\w+)'),
        )
        t = [n for n, p in patterns if len(re.compile(p).findall(line)) > 0]

        ret = {
            'content': line,
            'types': t,  # TODO: might be nicer to use a set for this
            'index': i,
        }

        # TODO: fix the regex. matches too much
        if 'assignment' in ret['types'] and 'function' in ret['types']:
            ret['types'].remove('assignment')

        block = ''
        # TODO: fix the regex. does not match enough
        if 'effect' in ret['types'] and 'function' not in ret['types']:
            ret['types'].append('function')

            if 'TWOD(' in ret['content']:
                block = parse_twod(name, content, ret, i)
            if 'XYZ(' in ret['content']:
                block = parse_xyz(name, content, ret, i)
        elif 'function' in ret['types']:
            if line.strip()[-1] == ';':
                ret['types'].remove('function')
                ret['types'].append('function_declaration')
            else:
                block = parse_function(name, content, ret, i)

        ret['block'] = block

        return ret

    return [analyze_line(i, line) for i, line in enumerate(content)]


def parse_twod(name, lines, line, index):
    return _parse(name, lines, line, index, twod_definition)


def twod_definition(name, line):
    ret = 'TWOD(effect_' + name + ') '

    if line['content'].strip()[-1] == '{':
        ret += ' {\n}'

    return ret


def parse_xyz(name, lines, line, index):
    return _parse(name, lines, line, index, xyz_definition)


def xyz_definition(name, line):
    ret = 'XYZ(effect_' + name + ') '

    if line['content'].strip()[-1] == '{':
        ret += ' {\n}'

    return ret


def parse_function(name, lines, line, index):
    return _parse(name, lines, line, index, function_definition)


def _parse(name, lines, line, index, definition):
    ret = definition(name, line)
    cc = line['content']

    if cc.startswith('#'):
        return ret

    begin_braces = cc.count('{')
    end_braces = 0
    offset = 0
    if not begin_braces:
        begin_braces, end_braces, offset = find_begin_braces(
            cc, index + 1
        )

    for i in range(index + 1 + offset, len(lines)):
        cc = lines[i]
        ret += cc
        begin_braces += cc.count('{')
        end_braces += cc.count('}')

        if begin_braces == end_braces:
            return ret

    return ret


def function_definition(name, line):
    ret = None

    if 'init' in line['types']:
        ret = 'static void init_' + name + '(void)'
    elif 'effect' in line['types']:
        ret = 'void effect_' + name + '(void)'
    else:
        return line['content']

    if '{' in line['content']:
        ret += ' {\n'

    return ret


def find_begin_braces(content, i):
    for j in range(i, len(content)):
        b = content.count('{')

        if b:
            return b, content.count('}'), j - i

    return 0, 0, 0
