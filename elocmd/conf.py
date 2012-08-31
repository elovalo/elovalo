import serial

# Serial connection configuration
PORT     = '/dev/serial/by-id/usb-Arduino__www.arduino.cc__0043_6493633303735101B170-if00'
BAUDRATE = 9600
TIMEOUT  = 1
PARITY   = serial.PARITY_NONE
BYTESIZE = serial.EIGHTBITS

# Program configurations
DEBUG = False

# Elovalo serial port protocol constants
ESCAPE             = '\x7e'
LITERAL_ESCAPE     = '\x00'

MODE_IDLE          = '\x00'
MODE_EFFECT        = '\x01'

CMD_STOP           = '\x01'
CMD_CHANGE_EFFECT  = '\x02'
CMD_SERIAL_FRAME   = '\x03'
CMD_SET_TIME       = '\x04'
CMD_GET_TIME       = '\x05'
CMD_SET_SENSOR     = '\x06'

RESP_REBOOT        = '\x01'
RESP_SWAP          = '\x02'
RESP_EFFECT_NAME   = '\x03'
RESP_EFFECT_END    = '\x04'
RESP_COMMAND_OK    = '\x05'
RESP_TIME          = '\x06'
RESP_INVALID_CMD   = '\xf0'
RESP_INVALID_ARG_A = '\xfa'
RESP_INVALID_ARG_B = '\xfb'
RESP_INVALID_ARG_C = '\xfc'
RESP_INVALID_ARG_D = '\xfd'
RESP_SHORT_PAYLOAD = '\xfe'
RESP_JUNK_CHAR     = '\xff'
