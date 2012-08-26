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


class SourceFiles(object):

    def __init__(self, files):
        pass

    @property
    def function_definitions(self):
        return 'defs'  # TODO

    @property
    def function_names(self):
        return 'names'  # TODO

    @property
    def effects(self):
        return 'effects'  # TODO

    @property
    def functions(self):
        return 'functions'  # TODO


def generate(source, target):
    inp = SourceFiles(glob(source + '/*.c'))

    with open(target, 'w') as t:
        t.write(file_start)
        t.write(inp.function_definitions)
        t.write(inp.function_names)
        t.write(inp.effects)
        t.write(inp.functions)
