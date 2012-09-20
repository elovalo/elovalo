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
# along with Elovalo.  If not, see <http:#www.gnu.org/licenses/>.
#

import serial

import configtuple

# Serial connection configuration
PORT     = ''
BAUDRATE = 9600
TIMEOUT  = 1
PARITY   = serial.PARITY_NONE
BYTESIZE = serial.EIGHTBITS

# Program configurations
DEBUG = False

# Elovalo serial port protocol constants
# Serial protocol fundamentals
Core = configtuple.ConfigTuple('Core', {
    'ESCAPE'          : '~',  # Escape character. Go to command mode
    'LITERAL_ESCAPE'  : '\0' # Escape followed by this is literal escape.
})

# Commands issued by the sender
Command = configtuple.ConfigTuple('Command', {
    'STOP'            : '.',
    'LIST_EFFECTS'    : 'e',
    'CHANGE_EFFECT'   : 'E',
    'SERIAL_FRAME'    : 'F',
    'GET_TIME'        : 't',
    'SET_TIME'        : 'T',
    'SET_SENSOR'      : 'S',
    'LIST_ACTIONS'    : 'a',
    'RUN_ACTION'      : 'A',
    'READ_CRONTAB'    : 'c',
    'WRITE_CRONTAB'   : 'C',
    'SELECT_PLAYLIST' : 'P',
    'NOTHING'         : '*' # May be used to end binary transmission
})


# Autonomous respons es. These may occur anywhere, anytime
Report = configtuple.ConfigTuple('Report', {
    'JUNK_CHAR'    : '@', # Unsolicited data received
    'INVALID_CMD'  : '?', # Unknown command type
    'ANSWERING'    : '(', # Command received, starting to process input
    'READY'        : ')', # Processing of command is ready
    'BOOT'         : 'B', # Device has been (re)booted
    'FLIP'         : '%' # Frame has been flipped, ready to receive new
})

# Typical answers to commands. Use of these is command-specific

Response = configtuple.ConfigTuple('Response', {
    'INTERRUPTED'    : 0x00,
    'BAD_ARG_A'      : 0x01,
    'BAD_ARG_B'      : 0x02
})

Cron = configtuple.ConfigTuple('Cron', {
    'ITEM_NOT_VALID' : 0x01
})

# Operating modes
Mode = configtuple.ConfigTuple('Mode', {
    'IDLE'           : 0x00, # Do not update display buffers
    'EFFECT'         : 0x01, # Draw effect
    'PLAYLIST'       : 0x02 # Playlist
})
