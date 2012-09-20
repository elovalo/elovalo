#!/usr/bin/env python
import sys


def main(args):
    if len(args) < 2:
        print 'Too few parameters. Try <path to assembly> <register number>'
        return

    assy = args[0]
    r_num = args[1]

    print assy, r_num


if __name__ == '__main__':
    main(sys.argv[1:])

