import binascii
import cmd
import sys
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

