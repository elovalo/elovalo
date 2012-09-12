/**
 * Ledikuutio Main...
 * TODO: 	Make sure that SRAM usage stays as low as possible,
 * 			avoid using local variables or declaring variables at runtime.
 *
 * TODO: 	Check if there's way to monitor the stack usage at runtime
 * 			to avoid stack overflow when pushing close the SRAM limit.
 *
 */

#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/io.h>
#include <stdlib.h>
#include "pinMacros.h"
#include "main.h"
#include "init.h"
#include "tlc5940.h"
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
#define REPORT_EFFECT_END   '.' // Running of effect has stopped

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
void process_cmd(void);
void send_escaped(uint8_t byte);
read_t read_escaped();
void report(uint8_t code);
bool answering(void);

int main() {
	cli();

	wdt_disable(); // To make sure nothing weird happens
	init_tlc5940();
	init_spi();

	init_blank_timer();
	init_effect_timer();

	initUSART();
	sei();

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

			// TODO: get length from playlist item
			ticks = centisecs();
			if (ticks > 1000) {
				// Rendered too long, stop.
				mode = MODE_IDLE;

				// Report to serial port
				report(REPORT_EFFECT_END);
				
				break;
			}

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
	} ELSEIFCMD(CMD_NOTHING) {
		// Nothing
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
		for (int8_t bit_pos=24; bit_pos>=0; bit_pos-=8) {
			read_t x = read_escaped();
			if (!x.good) goto interrupted;
			tmp_time |= (time_t)x.byte << bit_pos;
		}
		stime(&tmp_time);
	} ELSEIFCMD(CMD_GET_TIME) {
		time_t tmp_time = time(NULL);
		send_escaped(tmp_time >> 24);
		send_escaped(tmp_time >> 16);
		send_escaped(tmp_time >> 8);
		send_escaped(tmp_time);
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
		// TODO stub
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
				if (!x.good)
					goto interrupted;
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
void send_escaped(uint8_t byte) {
	serial_send(byte);
	if (byte == ESCAPE) serial_send(LITERAL_ESCAPE);
}

/**
 * Reads a byte. If it is a command, do not consume input. This uses
 * blocking reads.
 */
read_t read_escaped() {
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
void report(uint8_t code) {
	serial_send(ESCAPE);
	serial_send(code);
}

bool answering(void) {
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
