import time

import conf
import conn
import conv

class UnknownResponseException(conn.ElovaloException):
    pass

class Responses():

    def __init__(self, conn):
        self.conn = conn
        self._responses = {
            conf.RESP_REBOOT:       self.reboot,
            conf.RESP_TIME:         self.show_time,
            conf.RESP_EFFECT_NAMES: self.effect_names,
            conf.RESP_INVALID_CMD:  self.invalid_cmd,
            conf.RESP_COMMAND_OK:   self.do_nothing
        }

    def read(self):
        responses = self.conn.read_responses()
        for resp in responses:
            try:
                self._handle(resp)
            except UnknownResponseException, e:
                print(e)

    def _handle(self, resp):
        try:
            self._responses[resp.kind](resp.body)
        except KeyError:
            print('Error: No response handler for message {0}:{1}'.format(
                resp.kind, resp.body))
    
    def reboot(self, body):
        print('device rebooted')

    def show_time(self, timestamp):
        if len(timestamp) == 4:
            dt = conv.longToInt(timestamp[:4])
            dtstr = time.strftime('%a %d.%m.%Y - %H:%M:%S', time.localtime(dt))
            t = int(time.time())
            print("device time off by {0} seconds ({1})".format(dt - t, dtstr))
        else:
            print('Error: Received incorrect timestamp value from the device')
    
    def effect_names(self, name):
        pass #TODO Implement 

    def invalid_cmd(self, body):
        print('Received an invalid command, msg: {0}'.format(body))

    def do_nothing(self, body):
        pass
