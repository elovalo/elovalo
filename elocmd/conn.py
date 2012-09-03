import binascii
import collections
import serial
import sys

import conf

Message = collections.namedtuple('Message', 'kind body')

class Connection():
    def __init__(self):
        self.serial = serial.Serial(
            port=conf.PORT,
            baudrate=conf.BAUDRATE,
            timeout=conf.TIMEOUT,
            parity=conf.PARITY,
            bytesize=conf.BYTESIZE
        )

    def read_responses(self):
        resps = self._split_responses(self.serial.readline())
        return resps

    def _split_responses(self, raw_resps):
        if conf.DEBUG:
            print("Received: {0}".format(binascii.hexlify(raw_resps)))
        resps_data = filter(None, raw_resps.split(conf.ESCAPE))
        responses = []
        for resp_data in resps_data:
            resp = self._make_response(resp_data)
            if resp.kind != '':
                responses.append(self._make_response(resp_data))
        return responses
    
    def _make_response(self, resp_data):
        if len(resp_data) == 1:
            return Message(resp_data[0], '')
        elif len(resp_data) > 1:
            return Message(resp_data[0], resp_data[1:])
        else:
            return Message('', '')

    def send_message(self, kind, body=''):
        self.send(Message(kind, body))

    def send(self, msg):
        bytes_ = bytearray(conf.ESCAPE)
        bytes_.append(msg.kind)
        for i in range(0, len(msg.body)):
            bytes_.append(msg.body[i])
            if msg.body[i] == conf.ESCAPE:
                bytes_.append(conf.LITERAL_ESCAPE)

        self.send_raw(bytes_)

    def send_raw(self, raw):
        if conf.DEBUG:
            print("Sent: {0}".format(binascii.hexlify(raw)))
        self.serial.write(raw)
    
    def close(self):
        self.serial.close()
