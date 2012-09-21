import cmd
import readline
import struct
import sys
import time

import config
import connection
import parser

class EloCmd(cmd.Cmd):
    intro  = 'Type help to see available commands'
    prompt = ' > '

    conn = None
    
    effects = []
    actions = []

    def init(self):
        if self.conn is None:
            self.conn = connection.Connection()

    def preloop(self):
        self.init()
        r = self.response_parser()
        r.parse_response()
        time.sleep(2)
        self.load_effects()
        self.load_actions()

    def response_parser(self):
        line = self.conn.read()
        return parser.EloParser(line)

    def do_time(self, line):
        """Get and synchronize device time"""
        local_t = int(time.time())
        if line == "sync":
            t = struct.pack('<L', local_t)
            self.conn.send_command(config.Command.SET_TIME, t)
            return

        self.conn.send_command(config.Command.GET_TIME)
        device_t = self.response_parser().parse_time()

        dtstr = time.strftime('%a %d.%m.%Y - %H:%M:%S', device_t)
        print("device time off by {0} seconds ({1})".format(
            time.mktime(device_t) - local_t, dtstr))
    
    def complete_time(self, text, line, begidx, endidx):
        return ['sync']

    def do_stop(self, line):
        """Send stop-signal to the device"""
        self.conn.send_command(config.Command.STOP)

    def load_effects(self):
        self.conn.send_command(config.Command.LIST_EFFECTS)
        p = self.response_parser()
        self.effects = p.parse_effects()

    def do_effect(self, line):
        """Make the device run the specified action"""
        if line in self.effects:
            e = self.effects[line]
            self.conn.send_command(config.Command.LIST_EFFECTS, e)
        else:
            print("Unknown effect")

    def complete_effect(self, text, line, begidx, endidx):
        if text:
            return [effect for effect in self.effects.keys()
                    if effect.startswith(text)]
        else:
            return self.effects.keys()

    def load_actions(self):
        self.conn.send_command(config.Command.LIST_ACTIONS)
        p = self.response_parser()
        self.actions = p.parse_actions()

    def do_action(self, line):
        """Send the selected action with the specified byte parameter (0-255) to the device"""
        line = line.split(' ', 1)
        act_name = line[0]

        try:
            act_arg = chr(int(line[1]))
        except IndexError:
            act_arg = '\x00'
        except ValueError:
            print("Error: Incorrect argument")
            return

        if act_name in self.actions.keys():
            act = self.actions[act_name]
            self.conn.send_command(
                config.Command.RUN_ACTION,
                act.address + act_arg)

    def complete_action(self, text, line, begidx, endidx):
        if text:
            return [action for action in self.actions.keys()
                    if action.startswith(text)]
        else:
            return self.actions.keys()

    def do_cron(self, line):
        self.conn.send_command(config.Command.READ_CRONTAB)

    def do_quit(self, line):
        self.quit()

    def do_EOF(self, line):
        self.quit()

    def quit(self):
        self.conn.close()
        sys.exit()
