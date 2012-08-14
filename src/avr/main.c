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
#include <avr/io.h>
#include "pinMacros.h"
#include "main.h"
#include "init.h"
#include "tlc5940.h"
#include "serial.h"
#include "../cube.h"

#define ESCAPE              0x7e
#define LITERAL_ESCAPE      0x00

#define MODE_IDLE           0x00
#define MODE_EFFECT         0x01
#define MODE_SNAKE          0x02

#define CMD_STOP            0x00
#define CMD_CHANGE_EFFECT   0x01
#define CMD_SERIAL_FRAME    0x02
#define CMD_SNAKE           0x03

#define RESP_REBOOT         0x00
#define RESP_SWAP           0x01
#define RESP_JUNK_CHAR      0xfd
#define RESP_INVALID_CMD    0xfe
#define RESP_INVALID_EFFECT 0xff

uint8_t mode = MODE_IDLE;

// Private functions
void process_cmd(void);
void send_escaped(uint8_t byte);

int main() {
	cli();

	disableWDT();
	initPorts();
	initSPI();

	initTLC5940();
	init_blank_timer();
	init_effect_timer();

	initUSART();
	sei();

	InitGScycle(); //TODO: Send first byte to the SPI bus...

	// Greet the serial user
	serial_send(ESCAPE);
	serial_send(RESP_REBOOT);

	while(1) {
		if(serial_available()) {
			uint8_t cmd = serial_read();

			if (cmd == ESCAPE) {
				process_cmd();
			} else {
				// Errorneous command
				serial_send(ESCAPE);
				serial_send(RESP_JUNK_CHAR);
				send_escaped(cmd);
			}
		}

		switch (mode) {
		case MODE_IDLE:
			// No operation
			break;
		case MODE_EFFECT:
			// Execute an effect
			// Check if we should render at all
			// Get ticks
			// jump to effect code
			// flip buffers
			break;
		case MODE_SNAKE:
			// Snake effect
			animSnake();
			break;
		}
	}

	return 0;
}

void animSnake() {
	// Some variables used over multiple invocations
	static uint8_t i = 1;
	static uint8_t apu = 1; /* we need this in order to determine
				 * if the non-skipped number is odd */

	// Clear backbuffer once every frame...
	if (isAfterFlip) {
		clearArray(gs_buf_back, 24*TLC5940);
		
		if (i < (25*TLC5940)) {
			
			if(i%3==0) { //Skip!
				i++;
				apu=1; //we need to reset the helper
			}

			if(apu==1) { //Odd
				gs_buf_back[i-1]=0xff;
				gs_buf_back[i]=0xf0;
			} else { //even
				gs_buf_back[i-1]=0x0f;
				gs_buf_back[i]=0xff;
			}
			apu++;
		}
		
		i++;
		
		if(i==24*TLC5940){ //Ending cell, reset EVERYTHING
			i=1;
			apu=1;
			//clearArray(BackBuffer, 24*TLC5940);
		}
		
		isAfterFlip = 0;
	}
}

/**
 * Processes a command. The escape character is already read in main(). This may
 * block because of reading serial data, but that is okay for now.
 */
void process_cmd(void)
{
	uint8_t cmd = serial_read_blocking();

	switch (cmd) {
	case ESCAPE:
		// Put the character back
		serial_ungetc(ESCAPE);
		break;
	case CMD_STOP:
		mode = MODE_IDLE;
		break;
	case CMD_CHANGE_EFFECT:
		mode = MODE_EFFECT;
		// TODO read effect number
		break;
	case CMD_SERIAL_FRAME:
		mode = MODE_IDLE;
		// TODO read serial data
		break;
	case CMD_SNAKE:
		// Temporary "snake" effect used in debugging
		mode = MODE_SNAKE;
		break;
	default:
		// Report error
		serial_send(ESCAPE);
		serial_send(RESP_INVALID_CMD);
		send_escaped(cmd);
	}
}

void send_escaped(uint8_t byte) {
	serial_send(byte);
	if (byte == ESCAPE) serial_send(LITERAL_ESCAPE);
}

void clearArray(volatile uint8_t *arr, uint8_t len) {

	for (uint8_t r = 0; r < len; r++) {
		arr[r] = 0x00;
	}

}


//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{
	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
