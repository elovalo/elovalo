import binascii
import json
import time

import conf
import conn
import conv

class NoEffectException(conn.ElovaloException):
    pass

class IncorrectSensorDataException(conn.ElovaloException):
    pass

class Commands():

    def __init__(self, conn):
        self.conn = conn
        self.effects = {}
        self.get_effects()

    def get_effects(self):
        self.conn.send_message(conf.CMD_LIST_EFFECTS)
        resps = self.conn.read_responses()
        for r in resps:
            if r.kind == conf.RESP_EFFECT_NAMES:
                effects = filter(None, r.body.split('\0'))
                self._update_effects(effects)
                print self.effects

    def _update_effects(self, effects):
        for i in range(len(effects)):
            self.effects[effects[i]] = chr(i)

    def get_time(self):
        self.conn.send_message(conf.CMD_GET_TIME)

    def sync_time(self):
        t = conv.intToLong(int(time.time()))
        self.conn.send_message(conf.CMD_SET_TIME, t)

    def run_effect(self, effect):
        try:
            e = self.effects[effect]
            self._send_effect(e)
            return
        except KeyError:
            pass

        try:
            e = binascii.unhexlify(effect)
            self._send_effect(e)
        except TypeError:
            raise NoEffectException("No such effect, or incorrect hex value")

    def _send_effect(self, effect_id):
        self.conn.send_message(conf.CMD_CHANGE_EFFECT, effect_id)

    def send_sensor_data(self, filename):
        sensor_data = self._load_sensor_data(filename)
        for frame in sensor_data["frames"]:
            data = conv.sensor_data(frame)
            self._send_sensor_data(data)

    def _load_sensor_data(self, filename):
        f = open(filename)
        data = json.load(f)
        f.close()
        return data

    def _send_sensor_data(self, data):
        if len(data) % 2 != 0:
            raise IncorrectSensorDataException("Error: converted sensor data not dividable by 2")

        start = '\x00'
        ln    = '\x02'

        for i in range(0, len(data), 2):
            d1 = data[i]
            d2 = data[i+1]
            self.conn.send_message(conf.CMD_SET_SENSOR,
                                   start + ln + d1 + d2)

    def stop(self):
        self.conn.send_message(conf.CMD_STOP)
