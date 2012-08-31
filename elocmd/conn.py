import binascii
import collections
import serial
import sys

import conf

Response = collections.namedtuple('Response', 'kind body')

class Connection():
    def __init__(self):
        try:
            self.serial = serial.Serial(
                port=conf.PORT,
                baudrate=conf.BAUDRATE,
                timeout=conf.TIMEOUT,
                parity=conf.PARITY,
                bytesize=conf.BYTESIZE
            )
        except serial.SerialException:
            print("Could not open serial port connection, verify conf.py")
            sys.exit()

    def read_responses(self):
        resps = self._split_responses(self.serial.readline())
        return resps

    def _split_responses(self, raw_resps):
        if conf.DEBUG:
            print(binascii.hexlify(raw_resps))
        resps_data = filter(None, raw_resps.split(conf.ESCAPE))
        responses = []
        for resp_data in resps_data:
            resp = self._make_response(resp_data)
            if resp.kind != '':
                responses.append(self._make_response(resp_data))
        return responses
    
    def _make_response(self, resp_data):
        if len(resp_data) == 1:
            return Response(resp_data[0], '')
        elif len(resp_data) > 1:
            return Response(resp_data[0], resp_data[1:])
        else:
            return Response('', '')

    def send_command(self, bytestr):
        self.serial.write(conf.ESCAPE + bytestr)
    
    def close(self):
        self.serial.close()
