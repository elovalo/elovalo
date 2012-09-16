#!/usr/bin/env python
import os
import pep8

OKGREEN = '\033[92m'
ENDC = '\033[0m'


def walk(path, extensions, cb):
    path = os.path.abspath(path)

    for f in os.listdir(path):
        fp = os.path.join(path, f)
        p, ext = os.path.splitext(f)

        if ext[1:] in extensions:
            cb(fp)

        if os.path.isdir(fp):
            walk(fp, extensions, cb)

if __name__ == '__main__':
    def cb(path):
        print '\n' + OKGREEN + 'Checking ' + path + ENDC
        checker = pep8.Checker(path, show_source=True)
        errors = checker.check_all()

        if errors:
            print 'Found %s errors (and warnings)' % errors

    walk('..', ['py', ], cb)
