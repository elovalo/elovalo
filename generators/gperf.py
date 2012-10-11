#
# Copyright 2012 Elovalo project group
#
# This file is part of Elovalo.
#
# Elovalo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Elovalo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
#

import re
import subprocess

def pgm_wrap(var,s):
    """Wrap given var access inside pgm_get. Assumes return value of
    uint8_t in that var """
    m = re.search(var+'\[[^\]]',s)
    if m == None: return s
    start = m.start()
    brackets = 0
    for i in range(start,s.__len__()):
        if s[i] == '[':
            brackets += 1
        if s[i] == ']':
            brackets -= 1
            if brackets == 0:
                return s[:start]+'pgm_get('+s[start:i+1]+',byte)'+pgm_wrap(var,s[i+1:])

def generate(source, target):
    """This function runs gperf and mangles it output so it allows
    placing look-up table to PROGMEM. This implementation allows false
    negatives, i.e. if a glyph is not found then it returns
    anything."""

    # Get gperf output
    p = subprocess.Popen(["gperf",source],
                         stdout=subprocess.PIPE)
    out, err = p.communicate()
    if not p.returncode == 0:
        raise Exception('gperf failed with '+str(p.returncode))
    
    # Wrap asso_values and wordlist inside pgmspace and tune the
    # visibility of glyphs
    out = pgm_wrap('asso_values',out)
    out = out.replace("static const unsigned char asso_values",
                      "PROGMEM static const unsigned char asso_values")
    out = out.replace("static const struct glyph",
                      "PROGMEM const struct glyph")

    # Remove strings from wordlist, no need for verification
    out = re.sub(r'{".*",',r'{',out)
    out = out.replace('{""}','{}')
    out = out.replace('char *name;','')

    # Remove in_word_set to keep it compiling. It's not useful for us.
    out = out.replace('\nconst struct glyph *','\n#if 0')
    out = out + '\n#endif'

    # Write to target file
    f = open(target,'w')
    f.write(out)
    f.close()
