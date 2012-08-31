#!/usr/bin/python
import binascii
import cmd
import sys
import time

import conn
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

    def preloop(self):
        self.conn = conn.Connection()

    def postloop(self):
        self.conn.close()

    def do_time(self, param):
        'Get the current time difference from the device'
        self.conn.send_command(conf.CMD_GET_TIME)

    def do_sync(self, line):
        'Sync the device time to your computer time'
        t = conv.intToLong(int(time.time()))
        self.conn.send_command(conf.CMD_SET_TIME + t)

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
        self.conn.send_command(conf.CMD_CHANGE_EFFECT + e)

    def complete_effect(self, text, line, begidx, endidx):
        if not text:
            completions = self.EFFECTS.keys()
        else:
            completions = [f
                           for f in self.EFFECTS.keys()
                           if f.startswith(text)
                           ]
        return completions

    def do_stop(self, line):
        'Sets the device to idle-mode'
        self.conn.send_command(conf.CMD_STOP)

    def do_quit(self, line):
        'Quits this prompt'
        sys.exit()

    def do_EOF(self, line):
        return True

    def postcmd(self, stop, line):
        self._process_resps(self.conn.read_responses())
        return cmd.Cmd.postcmd(self, stop, line)

    def _process_resps(self, responses):
        for r in responses:
            resp.handle(r)

if __name__ == '__main__':
    EloCmd().cmdloop()
