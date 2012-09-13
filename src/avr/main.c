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

#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/io.h>
#include <stdlib.h>
#include "pinMacros.h"
#include "main.h"
#include "init.h"
#include "tlc5940.h"
#include "hcsr04.h"
#include "adc.h"
#include "serial.h"
#include "clock.h"
#include "configuration.h"
#include "../pgmspace.h"
#include "../cube.h"
#include "../effects.h"

/* Serial communication constants. Please note: With CMD and RESP
 * codes 0x00 and 0x7e are reserved for LITERAL_ESCAPE and ESCAPE,
 * respectively. */

// Protocol fundamentals
#define ESCAPE              '~'  // Escape character. Go to command mode
#define LITERAL_ESCAPE      '\0' // Escape followed by this is literal escape.

// Commands issued by the sender
#define CMD_STOP            '.'
#define CMD_LIST_EFFECTS    'e'
#define CMD_CHANGE_EFFECT   'E'
#define CMD_SERIAL_FRAME    'F'
#define CMD_GET_TIME        't'
#define CMD_SET_TIME        'T'
#define CMD_SET_SENSOR      'S'
#define CMD_LIST_ACTIONS    'a'
#define CMD_READ_CRONTAB    'c'
#define CMD_WRITE_CRONTAB   'C'
#define CMD_NOTHING         '*' // May be used to end binary transmission

// Autonomous responses. These may occur anywhere, anytime
#define REPORT_JUNK_CHAR    '@' // Unsolicited data received
#define REPORT_INVALID_CMD  '?' // Unknown command type
#define REPORT_ANSWERING    '(' // Command received, starting to process input
#define REPORT_READY        ')' // Processing of command is ready
#define REPORT_BOOT         'B' // Device has been (re)booted
#define REPORT_FLIP         '%' // Frame has been flipped, ready to receive new

// Typical answers to commands. Use of these is command-specific

#define RESP_INTERRUPTED    0x00
#define RESP_BAD_ARG_A      0x01
#define RESP_BAD_ARG_B      0x02

#define CRON_ITEM_NOT_VALID 0x01


// Operating modes
#define MODE_IDLE           0x00 // Do not update display buffers
#define MODE_EFFECT         0x01 // Draw effect

// Dirty tricks
#define ELSEIFCMD(CMD) else if (cmd==CMD && answering())

typedef struct {
	uint8_t good;
	uint8_t byte;
} read_t;

uint8_t mode = MODE_IDLE; // Starting with no operation on.
const effect_t *effect; // Current effect. Note: points to PGM

// Private functions
static void process_cmd(void);
static void send_escaped(uint8_t byte);
static read_t read_escaped();
static void report(uint8_t code);
static bool answering(void);
static void send_string_from_pgm(const char * const* pgm_p);

int main() {
	cli();

	wdt_disable(); // To make sure nothing weird happens
	init_tlc5940();
	init_spi();

	init_blank_timer();
	init_effect_timer();

	initUSART();
	sei();

	hcsr04_start_continuous_meas();
	adc_start();

	// Greet the serial user
	report(REPORT_BOOT);

	while(1) {
		if(serial_available()) {
			uint8_t cmd = serial_read();

			if (cmd == ESCAPE) process_cmd();
			else report(REPORT_JUNK_CHAR);
		}

		switch (mode) {
		case MODE_IDLE:
			// No operation
			break;
		case MODE_EFFECT: // TODO: playlist logic
			// If a buffer is not yet flipped
			if (may_flip) break;

			// Update clock and sensor values
			ticks = centisecs();
			sensors.distance1 = hcsr04_get_distance_in_cm();
			sensors.distance2 = hcsr04_get_distance_in_cm(); //TODO: use separate sensor
			sensors.ambient_light = adc_get(0) >> 2;
			sensors.sound_pressure_level = adc_get(1) >> 2;

			// Do the actual drawing
			draw_t draw = (draw_t)pgm_get(effect->draw,word);
			if (draw != NULL) {
				draw();
				// Mark buffer ready for swapping
				may_flip = 1;
			}

			break;
		}
	}

	return 0;
}

/**
 * Processes a command. The escape character is already read in
 * main(). This function may block because of reading serial data, but
 * that is okay for now.
 */
void process_cmd(void)
{
	uint8_t cmd = serial_read_blocking();

	if (cmd == ESCAPE) {
		// Put the character back
		serial_ungetc(ESCAPE);
		return; // Skip out:
	} if (cmd == CMD_NOTHING) {
		/* Outputs nothing, just ensures that previous command
		 * has ended */
		return;
	} ELSEIFCMD(CMD_STOP) {
		mode = MODE_IDLE;
	} ELSEIFCMD(CMD_CHANGE_EFFECT) {
		read_t x = read_escaped();

		if (!x.good) goto interrupted;
		if (x.byte >= effects_len) goto bad_arg_a;

		// Change mode and pick correct effect from the array.
		mode = MODE_EFFECT;
		effect = effects + x.byte;

		// Prevent flipping
		may_flip = 0;

		// Fetch init pointer from PROGMEM and run it
		init_t init = (init_t)pgm_get(effect->init, word);
		if (init != NULL) init();

		// Swap buffer to bring back buffer to front
		gs_buf_swap();
		
		/* Support NO_FLIP. Restore buffers to "normal state"
		   (pointing to different locations) and then "broke"
		   flipping if required by flip_buffers */
		gs_restore_bufs();
		if (pgm_get(effect->flip_buffers, byte) == NO_FLIP) {
			gs_buf_back = gs_buf_front;
		}

		// Restart tick counter
		reset_time();

	} ELSEIFCMD(CMD_SET_TIME) {
		time_t tmp_time = 0;
		for (uint8_t bit_pos=0; bit_pos<32; bit_pos+=8) {
			read_t x = read_escaped();
			if (!x.good) goto interrupted;
			tmp_time |= (time_t)x.byte << bit_pos;
		}
		stime(&tmp_time);
	} ELSEIFCMD(CMD_GET_TIME) {
		time_t tmp_time = time(NULL);
		send_escaped(tmp_time);
		send_escaped(tmp_time >> 8);
		send_escaped(tmp_time >> 16);
		send_escaped(tmp_time >> 24);
	} ELSEIFCMD(CMD_SET_SENSOR) {
		read_t start = read_escaped();
		read_t len = read_escaped();

		// Check we get bytes and not escapes
		if (!start.good || !len.good)
			goto interrupted;

		// Check boundaries
		if (start.byte >= sizeof(sensors_t))
			goto bad_arg_a;

		if (start.byte + len.byte > sizeof(sensors_t))
			goto bad_arg_b;

		// Filling buffer byte by byte
		uint8_t *p = (uint8_t*)&sensors;
		for (uint8_t i=start.byte; i<start.byte+len.byte; i++) {
			read_t x = read_escaped();
			if (!x.good)
				goto interrupted;
			p[i] = x.byte;
		}
	} ELSEIFCMD(CMD_LIST_EFFECTS) {
		// Report new effect name to serial user
		for (uint8_t i=0; i<effects_len; i++) {
			uint8_t *text_pgm = (uint8_t*)pgm_get(effects[i].name,word);
			uint8_t c;
			do {
				c = pgm_read_byte(text_pgm++);
				send_escaped(c);
			} while (c != '\0');
		}
	} ELSEIFCMD(CMD_LIST_ACTIONS) {
		// Report function pointers and their values.
		for (uint8_t i=0; i<cron_actions_len; i++) {
			// Send function pointer, little-endian
			uint16_t fp = pgm_get(cron_actions[i].act,word);
			send_escaped(fp);
			send_escaped(fp >> 8);

			// Send action and arg name
			send_string_from_pgm(&(cron_actions[i].act_name));
			send_string_from_pgm(&(cron_actions[i].arg_name));
		}
	} ELSEIFCMD(CMD_READ_CRONTAB) {
		for (uint8_t i=0; i<CRONTAB_SIZE; i++) {
			// Read one crotab entry
			struct event e;
			uint8_t *p = (uint8_t*)&e;

			get_crontab_entry(&e,i);
			if (e.kind == END) break; // from for
			// Send individual bytes
			for (int j=0; j<sizeof(struct event); j++) {
				send_escaped(p[j]);
			}
		}
	} ELSEIFCMD(CMD_WRITE_CRONTAB) {
		uint8_t i;
		for (i=0; i<CRONTAB_SIZE; i++) {
			// Writing a crontab entry
			struct event e;
			uint8_t *p = (uint8_t*)&e;
			
			// First is a special case
			read_t x = read_escaped();
			if (!x.good) {
				// Is okay if no data given
				break; // for
			}
			p[0] = x.byte;

			// Reading the rest
			for (int j=1; j<sizeof(struct event); j++) {
				x = read_escaped();
				if (!x.good) {
					send_escaped(RESP_INTERRUPTED);
					break;
				}
				p[j] = x.byte;
			}
			
			// Validating
			if (!is_event_valid(&e)) {
				send_escaped(CRON_ITEM_NOT_VALID);
				send_escaped(i);
				/* Break doesn't increment counter, so
				 * truncating works okay */
				break; // for
			}

			// Write to EEPROM
			set_crontab_entry(&e,i);
		}
		truncate_crontab(i); // Truncate crontab to data length
	} else {
		report(REPORT_INVALID_CMD);
		return;
	}
	goto out;

bad_arg_a:
	report(RESP_BAD_ARG_A);
	goto out;

bad_arg_b:
	report(RESP_BAD_ARG_B);
	goto out;

interrupted:
	report(RESP_INTERRUPTED);
	goto out;

out:
	// All commands should be ended, successful or not
	report(REPORT_READY);
}

/**
 * Sends a byte and escapes it if necessary.
 */
static void send_escaped(uint8_t byte) {
	serial_send(byte);
	if (byte == ESCAPE) serial_send(LITERAL_ESCAPE);
}

/**
 * Reads a byte. If it is a command, do not consume input. This uses
 * blocking reads.
 */
static read_t read_escaped() {
	read_t ret = {1,0};
	ret.byte = serial_read_blocking();

	if (ret.byte == ESCAPE) {
		ret.byte = serial_read_blocking();
		if (ret.byte != LITERAL_ESCAPE) {
			// Put bytes back and report that we got nothing.
			serial_ungetc(ret.byte);
			serial_ungetc(ESCAPE);
			ret.good = 0;
		}
	}
	return ret;
}

/**
 * Send response via serial port.
 */
static void report(uint8_t code) {
	serial_send(ESCAPE);
	serial_send(code);
}

static bool answering(void) {
	report(REPORT_ANSWERING);
	return true;
}

/**
 * Sends a string to serial port. Pointer must point to program memory
 * pointing to the actual string data in program memory. So, double
 * pointing and kinda complex types.
 */
static void send_string_from_pgm(const char * const* pgm_p)
{
	char *p = (char*)pgm_read_word_near(pgm_p);
	char c;

	// If is NULL, print is as zero-length string
	if ( p == NULL) {
		send_escaped('\0');
		return;
	}
	
	// Read byte-by-byte and write, including NUL byte
	do {
		c = pgm_read_byte_near(p++);
		send_escaped(c);
	} while (c != '\0');
}

//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{
	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
