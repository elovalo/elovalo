import struct

def intToLong(i):
    return struct.pack('>L', i)

def longToInt(l):
    return struct.unpack('>L', l)[0]
