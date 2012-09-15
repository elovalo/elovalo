#
# Copyright 2012 Elovalo project group 
# 
# This file is part of Elovalo.
# 
# Elovalo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Elovalo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
#

import serial

# Serial connection configuration
PORT     = ''
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
