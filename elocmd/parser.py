import binascii
import collections

import config

Action = collections.namedtuple('Action', 'address description arg_desc')

class EloParser():
    index = -1
    response = ''
    current = ''
    resp_data = ''

    def __init__(self, line):
        self.response = line

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
        self.resp_data = self.resp_data + b

    def unescape(self, data):
        return data.replace(
            config.Core.ESCAPE + config.Core.LITERAL_ESCAPE,
            config.Core.ESCAPE)

    def parse_response(self):
        while self.next_byte() != '':
            print self.current
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
        names = filter(None, self.resp_data.split('\x00'))
        effects = {}
        i = 0
        for name in names:
            effects[name] = i
            i += 1

        return effects

    def parse_actions(self):
        self.parse_response()
        actions = {}

        ac_data = self.resp_data.split('\x00')
        for i in range(0, len(ac_data), 3):
            try:
                key = ac_data[i][2:]
                ac = Action(ac_data[i][:2],
                            ac_data[i+1],
                            ac_data[i+2])
                actions[key] = ac
            except IndexError:
                break
        print actions
        return actions

    def parse_action(self):
        self.parse_response()
        return self.resp_data
        
