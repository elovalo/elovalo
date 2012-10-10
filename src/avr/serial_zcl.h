/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

#define ATI     'A'
#define ZCL_ACK 'K' // Successfully received last packet
#define ZCL_NAK 'N' // Error, please resend last packet
#define ZCL_STX 'S' // Sending a new packet
#define ZCL_CHAN 1 // ZCL message channel

/**
 * Reads serial port for ZCL frames
 */
void receive(void);

/**
 * Processes a ZCL packet, sends NAK if CRC check fails
 */
void process_packet(void);

/**
 * Processes a single ZCL message, returning its CRC code
 */
uint16_t process_message(uint16_t len);

/**
 * Reads a packets length from serial port
 */
uint16_t read_packet_length(void);

/**
 * Checks if an ATI response is being sent,
 * and responds with identity information if one is found
 */
void check_ATI(void);

/**
 * Send a NAK error frame to serial port
 */
void send_error(void);

/**
 * Send ACK frame to serial port, signaling that the message was received successfully
 */
void send_ok(void);
