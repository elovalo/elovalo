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
import cmd
import json
import os
import sys
import struct
import time

import conn
from conn import Message
import conf
import conv
import resp

class EloCmd(cmd.Cmd):

    EFFECTS = {
        'game_of_life': '\x01',
        'heart':        '\x02',
        'brownian':     '\x03',
        'sine':         '\x04',
        'wave':         '\x05',
        'sphere':       '\x06',
        'worm':         '\x07',
        'const':        '\x08',
        'layers':       '\x09',
        'all_on':       '\x0a',
    }

    intro  = 'Type help to see available commands'
    prompt = ' > '

    conn = None

    def preloop(self):
        if not self.conn:
            self.conn = conn.Connection()
            self.conn.send_message(conf.CMD_GET_TIME)
            self._process_resps(self.conn.read_responses())

    def postloop(self):
        self.conn.close()

    def run_cmd(self, s):
        self.precmd(s)
        self.onecmd(s)
        self.postcmd(False, s)

    def do_time(self, param):
        'Get the current time difference from the device'
        self.conn.send_message(conf.CMD_GET_TIME)
        return True

    def do_sync(self, line):
        'Sync the device time to your computer time'
        t = conv.intToLong(int(time.time()))
        self.conn.send_message(conf.CMD_SET_TIME, t)
        return True

    def do_effect(self, effect):
        "Runs an effect on the device, accepts either effect name, or a 'f1' formatted hexadecimal effect number"
        e = ''
        if  effect in self.EFFECTS:
            e = self.EFFECTS[effect]
        elif len(effect) == 2:
            try:
                e = binascii.unhexlify(effect)
            except TypeError:
                print('Incorrect hex effect')
        else:
            print('No such effect')
            return
        self.conn.send_message(conf.CMD_CHANGE_EFFECT, e)

    def complete_effect(self, text, line, begidx, endidx):
        if not text:
            completions = self.EFFECTS.keys()
        else:
            completions = [f
                           for f in self.EFFECTS.keys()
                           if f.startswith(text)
                           ]
        return completions

    def do_sensor(self, sensor_file):
        f = open(sensor_file)
        sensor_data = json.load(f)
        f.close()
        self._send_sensor_data(sensor_data["frames"][0])

    def _send_sensor_data(self, data):
        sdata = conv.sensor_data(data)
        start = '\x00'
        ln = '\x02'

        for i in range(0, len(sdata), 2):
            try:
                d1 = sdata[i]
                d2 = sdata[i+1]
                self.conn.send_message(conf.CMD_SET_SENSOR,
                    start + ln + d1 + d2)
            except IndexError:
                if conf.DEBUG:
                    print("Error: Sensor data not dividable by 2")

    def complete_sensor(self, text, line, begidx, endidx):
        if not text:
            return os.listdir(os.curdir)
        else:
            return [f for f in os.listdir(os.curdir) if f.startswith(text)]
    
    def do_stop(self, line):
        'Sets the device to idle-mode'
        self.conn.send_message(conf.CMD_STOP)

    def do_quit(self, line):
        'Quits this prompt'
        sys.exit()

    def do_EOF(self, line):
        return True

    def precmd(self, line):
        if not self.conn:
            self.conn = conn.Connection()
            self._process_resps(self.conn.read_responses())
        return cmd.Cmd.precmd(self, line)

    def postcmd(self, stop, line):
        self._process_resps(self.conn.read_responses())
        return False

    def _process_resps(self, responses):
        for r in responses:
            resp.handle(r)
