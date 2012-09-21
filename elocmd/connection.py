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
# along with Elovalo.  If not, see <http:#www.gnu.org/licenses/>.
#

import binascii
import serial
import time

import config

class Connection():
    def __init__(self):
        self.ser = serial.Serial(
            port     = config.PORT,
            baudrate = config.BAUDRATE,
            timeout  = config.TIMEOUT,
            parity   = config.PARITY,
            bytesize = config.BYTESIZE
        )

    def send_command(self, kind, body=bytes()):
        if kind not in config.Command:
            print("Error: Unknown command given")
            return

        cmd = bytearray(config.Core.ESCAPE)
        cmd.append(kind)
        try:
            for i in body:
                cmd.append(i)
                if i == config.Core.ESCAPE:
                    cmd.append(config.Core.LITERAL_ESCAPE)
        except TypeError:
            cmd.append(body)

        if config.DEBUG:
            print("Sent: {0} | {1}".format(cmd, binascii.hexlify(cmd)))

        self.ser.write(cmd)

    def read(self):
        time.sleep(0.5)
        r = self.ser.read(self.ser.inWaiting())
        if config.DEBUG:
            print("Received: {0} | {1}".format(r, binascii.hexlify(r)))
        return r

    def close(self):
        self.ser.close()
