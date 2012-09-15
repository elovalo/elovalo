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

import binascii
import struct

import conf

def intToLong(i):
    return struct.pack('>L', i)

def longToInt(l):
    return struct.unpack('>L', l)[0]

def sensor_data(data):
    if len(data) % 2 != 0:
        print("Error: Sensor data length not dividable by 2")
        return ''

    max_val = pow(2, 12) - 1
    vals = ''
    for i in xrange(0, len(data)):
        b = binascii.hexlify(struct.pack('>H', int(data[i] * max_val)))
        vals = vals + b[1:]
    return binascii.unhexlify(vals)

def escape_byte(b):
    if b == conf.ESCAPE:
        return b + conf.LITERAL_ESCAPE
    return b
