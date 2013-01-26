#!/usr/bin/python
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
import collections
import struct
import time

import config

Action = collections.namedtuple('Action', 'address description arg_desc')

class EloParser():

    def __init__(self, line):
        self.response = line
        self.index = -1
        self.current = ''
        self.resp_data = bytearray()

    def next_byte(self):
        try:
            self.index += 1
            self.current = self.response[self.index]
            return self.current
        except IndexError:
            return ''

    def backup(self):
        try:
            self.index -= 1
            self.current = self.response[self.index]
            return self.current
        except IndexError:
            return ''

    def peek(self):
        b = self.next_byte()
        self.backup()
        return b

    def add_data(self, b):
        self.resp_data.append(b)

    def resp_string(self):
        return str(self.resp_data)

    def unescape(self, data):
        return data.replace(
            config.Core.ESCAPE + config.Core.LITERAL_ESCAPE,
            config.Core.ESCAPE)

    def parse_response(self):
        while self.next_byte() != '':
            if self.current == config.Core.ESCAPE:
                self.parse_esc()

        return self.resp_data

    def parse_esc(self):
        self.next_byte()
        if self.current == config.Report.JUNK_CHAR:
            print("Unknown character")
        elif self.current == config.Report.INVALID_CMD:
            print("Invalid command")
        elif self.current == config.Report.BOOT:
            print("Device rebooted")
        elif self.current == config.Report.ANSWERING:
            self.parse_data()

    def parse_data(self):
        while self.next_byte() != '':
            if self.current == config.Core.ESCAPE:
                p = self.next_byte()
                if p == config.Core.LITERAL_ESCAPE:
                    self.add_data(config.Core.ESCAPE)
                elif p == config.Report.READY:
                    return
                else:
                    self.backup()
                    self.parse_esc()
            else:
                self.add_data(self.current)

    def parse_effects(self):
        self.parse_response()
        names = filter(None, self.resp_string().split('\x00'))
        effects = {}
        i = 0
        for name in names:
            effects[name] = i
            i += 1

        return effects

    def parse_actions(self):
        self.parse_response()
        actions = {}

        ac_data = self.resp_string().split('\x00')
        for i in range(0, len(ac_data), 3):
            try:
                key = ac_data[i][2:]
                ac = Action(ac_data[i][:2],
                            ac_data[i+1],
                            ac_data[i+2])
                actions[key] = ac
            except IndexError:
                break
        return actions

    def parse_action(self):
        self.parse_response()
        return self.resp_string()

    def parse_time(self):
        self.parse_response()
        try:
            print "parse_time"
            print binascii.hexlify(self.resp_data)
            dt = struct.unpack('<L', self.resp_string()[:4])[0]
            print dt
            return time.localtime(float(dt))
        except IndexError:
            print("Received time value was too short")
            return time.localtime(0)
    
    def parse_flip(self):
        self.parse_response()
        r = self.resp_data
        print("RESPONSE: {}".format(r))
        return '%' in r

    def parse_ok(self):
        self.parse_response()
        r = self.resp_data
        if config.Response.INTERRUPTED in r:
            print('Command interrupted')
            return False
        if config.Response.BAD_ARG_A in r:
            print('Bad argument A')
            return False
        if config.Response.BAD_ARG_B in r:
            print('Bad argument B')
            return False
        return True
