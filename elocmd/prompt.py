import cmd
import json
import os
import sys

import commands
import conn
import resp

class EloCmd(cmd.Cmd):

    intro  = 'Type help to see available commands'
    prompt = ' > '

    _initialized = False
    conn = None

    def _init(self):
        if self._initialized:
            return

        self.conn = conn.Connection()
        self.commands = commands.Commands(self.conn)
        self._initialized = True

    def preloop(self):
        self._init()

    def postloop(self):
        self.conn.close()

    def run_cmd(self, s):
        self.precmd(s)
        self.onecmd(s)
        self.postcmd(False, s)

    def do_time(self, param):
        'Get the current time difference from the device'
        self.commands.get_time()
        return True

    def do_sync(self, line):
        'Sync the device time to your computer time'
        self.commands.sync_time()
        return True

    def do_effect(self, effect):
        "Runs an effect on the device, accepts either effect name, or a 'f1' formatted hexadecimal effect number"
        try:
            self.commands.run_effect(effect)
        except commands.NoEffectException, e:
            print(e)

    def complete_effect(self, text, line, begidx, endidx):
        if not text:
            completions = self.commands.EFFECTS.keys()
        else:
            completions = [f
                           for f in self.commands.EFFECTS.keys()
                           if f.startswith(text)
                           ]
        return completions

    def do_sensor(self, sensor_file):
        try:
            self.commands.send_sensor_data(sensor_file)
        except IOError:
            print("Could not open sensor data file")

    def complete_sensor(self, text, line, begidx, endidx):
        if not text:
            return os.listdir(os.curdir)
        else:
            return [f for f in os.listdir(os.curdir) if f.startswith(text)]
    
    def do_stop(self, line):
        'Sets the device to idle-mode'
        self.commands.stop()

    def do_quit(self, line):
        'Quits this prompt'
        sys.exit()

    def do_EOF(self, line):
        #TODO: Exit on EOF
        return True

    def precmd(self, line):
        self._init()
        return cmd.Cmd.precmd(self, line)

    def postcmd(self, stop, line):
        self._process_resps(self.conn.read_responses())
        return False

    def _process_resps(self, responses):
        for r in responses:
            resp.handle(r)
