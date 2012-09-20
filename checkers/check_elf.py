#!/usr/bin/env python
import sys


def main(args):
    if len(args) < 2:
        print 'Too few parameters. Try <path to assembly> <register number>'
        return

    assy = args[0]
    reg = args[1]

    with open(assy, 'r') as f:
        name = ''

        for line in f.readlines():
            if not line.startswith(' '):
                name = line

            if line.find(reg + ',') >= 0 or \
                    line.find(reg + '\n') >= 0:
                print name, line


if __name__ == '__main__':
    main(sys.argv[1:])

