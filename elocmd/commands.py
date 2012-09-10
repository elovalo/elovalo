import binascii
import json
import time

import conf
import conv

class ElovaloException(BaseException):
    pass

class NoEffectException(ElovaloException):
    pass

class IncorrectSensorDataException(ElovaloException):
    pass

class Commands():

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

    def __init__(self, conn):
        self.conn = conn

    def get_time(self):
        self.conn.send_message(conf.CMD_GET_TIME)

    def sync_time(self):
        t = conv.intToLong(int(time.time()))
        self.conn.send_message(conf.CMD_SET_TIME, t)

    def run_effect(self, effect):
        try:
            e = self.EFFECTS[effect]
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
