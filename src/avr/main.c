/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/io.h>
#include <stdlib.h>
#include "pinMacros.h"
#include "init.h"
#include "tlc5940.h"
#include "hcsr04.h"
#include "adc.h"
#include "serial.h"
#include "serial_escaped.h"
#include "clock.h"
#include "configuration.h"
#include "powersave.h"
#include "../pgmspace.h"
#include "../cube.h"
#include "../effects.h"
#include "../playlists.h"

/* Serial communication constants. Please note when allocating new CMD
 * and REPORT codes: LITERAL_ESCAPE and ESCAPE should not be used. See
 * the values in serial_escaped.h .
 */

// Commands issued by the sender
#define CMD_STOP            '.'
#define CMD_LIST_EFFECTS    'e'
#define CMD_CHANGE_EFFECT   'E'
#define CMD_SERIAL_FRAME    'F'
#define CMD_GET_TIME        't'
#define CMD_SET_TIME        'T'
#define CMD_SET_SENSOR      'S'
#define CMD_LIST_ACTIONS    'a'
#define CMD_RUN_ACTION      'A'
#define CMD_READ_CRONTAB    'c'
#define CMD_WRITE_CRONTAB   'C'
#define CMD_SELECT_PLAYLIST 'P'
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
#define MODE_PLAYLIST       0x02 // Playlist

// Dirty trick to ease building of CMD handling blocks
#define ELSEIFCMD(CMD) else if (cmd==CMD && answering())
// Fills in a variable and leaves handler if it cannot be read
#define SERIAL_READ(x) if (serial_to_sram(&(x),sizeof(x)) < sizeof(x)) goto interrupted

uint8_t mode = MODE_IDLE; // Starting with no operation on.
const effect_t *effect; // Current effect. Note: points to PGM

uint16_t effect_length; // Length of the current effect. Used for playlist
// It might be nice to use this for single effect too (set via serial).
uint8_t active_effect; // Index of the active effect. Used for playlist


// Private functions
static void process_cmd(void);
static void report(uint8_t code);
static bool answering(void);
static void init_playlist(void);
static void next_effect();
static void select_playlist_item(uint8_t index);
static void init_current_effect(void);

int main() {
	cli();

	wdt_disable(); // To make sure nothing weird happens
	init_tlc5940();
	init_spi();
	init_ps();

	init_blank_timer();
	init_effect_timer();
	
	init_playlist();
	
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
		case MODE_PLAYLIST:
			ticks = centisecs();
			if (ticks > effect_length) {
				next_effect();
				init_current_effect();
			}

			// no need to break!
			// fall to MODE_EFFECT on purpose
		case MODE_EFFECT:
			// If a buffer is not yet flipped
			if (flags.may_flip) break;

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
				allow_flipping(true);
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
		uint8_t i;
		SERIAL_READ(i);
		if (i >= effects_len)
			goto bad_arg_a;

		// Change mode and pick correct effect from the array.
		mode = MODE_EFFECT;
		effect = effects + i;

		// Prepare running of the new effect
		init_current_effect();
	} ELSEIFCMD(CMD_SELECT_PLAYLIST) {
		uint8_t i;
		SERIAL_READ(i);
		if (i >= playlists_len)
			goto bad_arg_a;

		// Change mode and run init
		mode = MODE_PLAYLIST;
		select_playlist_item(playlists[i]);
		init_current_effect();
	} ELSEIFCMD(CMD_SET_TIME) {
		time_t tmp_time;
		SERIAL_READ(tmp_time);
		stime(&tmp_time);
	} ELSEIFCMD(CMD_GET_TIME) {
		time_t tmp_time = time(NULL);
		sram_to_serial(&tmp_time,sizeof(time_t));
	} ELSEIFCMD(CMD_SET_SENSOR) {
		struct {
			uint8_t start;
			uint8_t len;
		} data;
		SERIAL_READ(data);

		// Check boundaries
		if (data.start >= sizeof(sensors_t))
			goto bad_arg_a;

		if (data.start + data.len > sizeof(sensors_t))
			goto bad_arg_b;

		// Fill in sensor structure
		void *p = &sensors;
		if (serial_to_sram(p+data.start,data.len) < data.len)
			goto interrupted;
	} ELSEIFCMD(CMD_LIST_EFFECTS) {
		// Print effect names separated by '\0' character
		for (uint8_t i=0; i<effects_len; i++) {
			send_string_from_pgm(&effects[i].name);
		}
	} ELSEIFCMD(CMD_LIST_ACTIONS) {
		// Report function pointers and their values.
		for (uint8_t i=0; i<cron_actions_len; i++) {
			// Send function pointer, little-endian
			uint16_t fp = pgm_get(cron_actions[i].act,word);
			send_escaped(fp);
			send_escaped(fp >> 8);

			// Send action and arg name
			send_string_from_pgm(&cron_actions[i].act_key);
			send_string_from_pgm(&cron_actions[i].act_name);
			send_string_from_pgm(&cron_actions[i].arg_name);
		}
	} ELSEIFCMD(CMD_RUN_ACTION) {
		// Runs given action immediately
		struct {
			action_t act;
			uint8_t arg;
		} a;
		SERIAL_READ(a);
		if (!is_action_valid(a.act))
			goto bad_arg_a;
		a.act(a.arg);
	} ELSEIFCMD(CMD_READ_CRONTAB) {
		// Print all entries in crontab
		for (uint8_t i=0; i<CRONTAB_SIZE; i++) {
			// Read one crotab entry from EEPROM
			struct event e;
			get_crontab_entry(&e,i);

			// If it's end, stop reading, otherwise send bytes
			if (e.kind == END) break; // from for
			sram_to_serial(&e,sizeof(e));
		}
	} ELSEIFCMD(CMD_WRITE_CRONTAB) {
		uint8_t i;
		for (i=0; i<CRONTAB_SIZE; i++) {
			// Writing a crontab entry
			struct event e;

			/* Read a single element. If user stops
			 * sending in element boundary, that is
			 * acceptable. If not, then report error. */
			uint8_t bytes_read = serial_to_sram(&e,sizeof(e));
			if (bytes_read == 0) {
				break; // It's okay to stop in element boundary
			} else if (bytes_read < sizeof(e)) {
				send_escaped(RESP_INTERRUPTED);
				break;
			}
			
			// Validating
			if (!is_event_valid(&e)) {
				send_escaped(CRON_ITEM_NOT_VALID);
				send_escaped(i);
				break;
			}

			// Write to EEPROM
			set_crontab_entry(&e,i);
		}

		/* Truncating crontab to element count. This is done
		 * even in case of an error. In that case the crontab
		 * is truncated to the length of first valid
		 * entries. */
		truncate_crontab(i);
	} else {
		report(REPORT_INVALID_CMD);
		return;
	}
	goto out;

bad_arg_a:
	send_escaped(RESP_BAD_ARG_A);
	goto out;

bad_arg_b:
	send_escaped(RESP_BAD_ARG_B);
	goto out;

interrupted:
	send_escaped(RESP_INTERRUPTED);
	goto out;

out:
	// All commands should be ended, successful or not
	report(REPORT_READY);
}

static void init_playlist(void) {
	select_playlist_item(0);
}

static void next_effect() {
	if(active_effect + 1 == master_playlist_len) select_playlist_item(0);
	else select_playlist_item(active_effect + 1);
}

static void select_playlist_item(uint8_t index) {
	active_effect = index;
	const playlistitem_t *item = master_playlist + index;
	uint8_t e_id = pgm_get(item->id,byte);
	effect = effects + e_id;
	effect_length = pgm_get(item->length,word);
	custom_data = pgm_get(item->data,word);
}

static void init_current_effect(void) {
	// Disable flipping until first frame is drawn
	allow_flipping(false);

	/* Restore front and back buffer pointers to point to
	 * different locations */
	gs_restore_bufs();

	// Run initializer
	init_t init = (init_t)pgm_get(effect->init, word);
	if (init != NULL) init();
	gs_buf_swap();
	
	/* If NO_FLIP, we "broke" flipping if required by pointing
	 * both buffers to the same location */
	if (pgm_get(effect->flip_buffers, byte) == NO_FLIP) {
		gs_buf_back = gs_buf_front;
	}
	
	// Restart tick counter
	reset_time();
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

//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{
	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
