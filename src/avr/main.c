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
#include "timer.h"
#include "../pgmspace.h"
#include "../cube.h"
#include "../effects.h"

#define ESCAPE              0x7e
#define LITERAL_ESCAPE      0x00

#define MODE_IDLE           0x00
#define MODE_EFFECT         0x01

#define CMD_STOP            0x01
#define CMD_CHANGE_EFFECT   0x02
#define CMD_SERIAL_FRAME    0x03

#define RESP_REBOOT         0x01
#define RESP_SWAP           0x02
#define RESP_EFFECT_NAME    0x03
#define RESP_EFFECT_END     0x04
#define RESP_JUNK_CHAR      0xfd
#define RESP_INVALID_CMD    0xfe
#define RESP_INVALID_EFFECT 0xff

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
void dislike(uint8_t error_code, uint8_t payload);

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
	serial_send(ESCAPE);
	serial_send(RESP_REBOOT);

	while(1) {
		if(serial_available()) {
			uint8_t cmd = serial_read();

			if (cmd == ESCAPE) process_cmd();
			else dislike(RESP_JUNK_CHAR,cmd);
		}

		switch (mode) {
		case MODE_IDLE:
			// No operation
			break;
		case MODE_EFFECT:
			// If a buffer is not yet flipped
			if (may_flip) break;

			ticks = centisecs();
			if (ticks > pgm_get(effect->length,word)) {
				// Rendered too long, stop.
				mode = MODE_IDLE;

				// Report to serial port
				serial_send(ESCAPE);
				serial_send(RESP_EFFECT_END);
				
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
	read_t x;

	switch (cmd) {
	case ESCAPE:
		// Put the character back
		serial_ungetc(ESCAPE);
		break;
	case CMD_STOP:
		mode = MODE_IDLE;
		break;
	case CMD_CHANGE_EFFECT:
		x = read_escaped();

		if (!x.good) break;

		if (x.byte >= effects_len) {
			dislike(RESP_INVALID_EFFECT,cmd);
			break;
		}

		// Change mode and pick correct effect from the array.
		mode = MODE_EFFECT;
		effect = effects + x.byte;

		// Report new effect name to serial user
		serial_send(ESCAPE);
		serial_send(RESP_EFFECT_NAME);
		uint8_t *text_pgm = (uint8_t*)pgm_get(effect->name,word);
		uint8_t c;
		do {
			c = pgm_read_byte(text_pgm++);
			send_escaped(c);
		} while (c != '\0');

		// Prevent flipping
		may_flip = 0;

		// Fetch init pointer from PROGMEM and run it
		init_t init = (init_t)pgm_get(effect->init, word);
		if (init != NULL) init();

		// Swap buffer to bring back buffer to front
		gs_buf_swap(); 
		
		// Restart tick counter
		reset_time();

		break;
	case CMD_SERIAL_FRAME:
		mode = MODE_IDLE;
		// TODO read serial data
		break;
	default:
		dislike(RESP_INVALID_CMD,cmd);
	}
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
 * Send error to user via serial port.
 */
void dislike(uint8_t error_code, uint8_t payload) {
	serial_send(ESCAPE);
	serial_send(error_code);
	send_escaped(payload);
}

//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{
	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
