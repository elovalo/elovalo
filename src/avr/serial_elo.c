/* Serial communication constants. Please note when allocating new CMD
 * and REPORT codes: LITERAL_ESCAPE and ESCAPE should not be used. See
 * the values in serial_escaped.h .
 */

#ifdef AVR_ELO

#include <avr/io.h>
#include <stdlib.h>

#include "main.h"
#include "configuration.h"
#include "serial.h"
#include "serial_escaped.h"
#include "../common/pgmspace.h"
#include "serial_elo.h"

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
#define REPORT_JUNK_CHAR   '@' // Unsolicited data received
#define REPORT_INVALID_CMD '?' // Unknown command type
#define REPORT_ANSWERING   '(' // Command received, starting to process input
#define REPORT_READY       ')' // Processing of command is ready
#define REPORT_BOOT        'B' // Device has been (re)booted
#define REPORT_FLIP        '%' // Frame has been flipped, ready to receive new

// Typical answers to commands. Use of these is command-specific

#define RESP_INTERRUPTED 0x00
#define RESP_BAD_ARG_A   0x01
#define RESP_BAD_ARG_B   0x02

#define CRON_ITEM_NOT_VALID 0x01

// Dirty trick to ease building of CMD handling blocks
#define ELSEIFCMD(CMD) else if (cmd==CMD && answering())
// Fills in a variable and leaves handler if it cannot be read
#define SERIAL_READ(x) if (serial_to_sram(&(x),sizeof(x)) < sizeof(x)) goto interrupted


static void report(uint8_t code);
static uint8_t answering(void);

/**
 * Reports that the device has booted,
 * Ran before entering the main loop
 */
void serial_elo_init(void) {
	report(REPORT_BOOT);
}

/**
 * Processes a command. The escape character is already read in
 * main(). This function may block because of reading serial data, but
 * that is okay for now.
 */
void serial_elo_process(uint8_t cmd) {
	if (cmd != ESCAPE) {
		report(REPORT_JUNK_CHAR);
		return;
	}

	cmd = serial_read_blocking();

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
		if ( change_current_effect(i) ) {
			goto bad_arg_a;
		}
	} ELSEIFCMD(CMD_SELECT_PLAYLIST) {
		uint8_t i;
		SERIAL_READ(i);
		if ( change_playlist(i) ) {
			goto bad_arg_a;
		}
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

/**
 * Send response via serial port.
 */
static void report(uint8_t code) {
	serial_send(ESCAPE);
	serial_send(code);
}

static uint8_t answering(void) {
	report(REPORT_ANSWERING);
	return 1;
}

#endif // AVR_ELO
