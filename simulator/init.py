import os
import sys
from subprocess import call

os.environ['effect'] = sys.argv[1]
os.environ['path'] = sys.argv[2]

os.chdir('..')
call('scons', shell=True)
os.chdir('simulator')
call('../build_exporter/exporter', shell=True)

