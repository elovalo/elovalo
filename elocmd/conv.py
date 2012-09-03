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
