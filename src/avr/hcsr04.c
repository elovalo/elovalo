
#include <stdio.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <inttypes.h>
#include "hcsr04.h"

//values for state
#define ST_IDLE 0
#define ST_SENDING_START_PULSE 1
#define ST_WAITING_RESPONSE_PULSE 2
#define ST_MEASURING_RESPONSE_PULSE 3
#define ST_WAITING_ECHO_FADING_AWAY 4

volatile uint8_t state;

volatile uint16_t resp_pulse_length;

//TODO: hmm, elovalo has a common init.c, move this into it
void timer_init(void){
	// Set up interrupt
	//TCCR1A &= ~(1<<WGM11 | 1<<WGM10); //no need to do this, bits are cleared by default
	//TCCR1B &= ~(1<<WGM13 | 1<<WGM12); //no need to do this, bits are cleared by default
	TCCR1B |= (1<<WGM12);  // sets compare and reset to "top" mode
	TCCR1B |= (1<<CS10);   // set clock divider to 1
	OCR1A = 160;           // set "top" for 10 us (16 MHz sysclock in use)
	TIFR1 |= (1<<OCF1A);   // clear possible pending int
	TIMSK1 |= (1<<OCIE1A); // enable int

	state = ST_IDLE;
}

ISR(TIMER1_COMPA_vect) {

	if (state == ST_SENDING_START_PULSE){

		PORTC &= ~(1<<PC5);    // force trailing edge of the pulse

		//re-init TIMER1 for measuring response pulse length
		TCCR1B &= ~(1<<CS12 | 1<<CS11 | 1<<CS10);   // stop timer
		OCR1A = 6000;           // set "top" for 96 ms (16 MHz sysclock in use)
		TCNT1 = 0;
		TCCR1B |= (1<<CS12);   // start timer, set clock divider to 256

		state = ST_WAITING_RESPONSE_PULSE;
	}
	else {
		//normally the state should be ST_WAITING_ECHO_FADING_AWAY here
		//but clear state machine also in all other cases

		if (state == ST_MEASURING_RESPONSE_PULSE){
			//This situation seems to happen in the next measurement cycle
			//after an object has disappeared from the beam area
			resp_pulse_length = HCSR04_MEAS_FAIL;
		}

		TIMSK1 &= ~(1<<OCIE1A); // disable int
		PCICR &= ~(1<<PCIE1); //Disable PIN Change Interrupt 1
		state = ST_IDLE;
	}
}

ISR(PCINT1_vect){

	if (state == ST_WAITING_RESPONSE_PULSE){
		resp_pulse_length = 0 - TCNT1; //underflow on purpose
		state = ST_MEASURING_RESPONSE_PULSE;
	}
	else if (state == ST_MEASURING_RESPONSE_PULSE){
		resp_pulse_length += TCNT1;
		PCICR &= ~(1<<PCIE1); //Disable PIN Change Interrupt 1
		state = ST_WAITING_ECHO_FADING_AWAY;
	}
	//else{
		//should not happen
	//}
}

//TODO: hmm, elovalo has a common init.c, move this into it
void pin_init(void){
	DDRC |= (1<<PC5); //output
	DDRC &= ~(1<<PC4); //input

	//Enable PIN Change Interrupt 1 - This enables interrupts on pins
	//PCINT14...8, see doc8161.pdf Rev. 8161D – 10/09, ch 12
	PCICR |= (1<<PCIE1);

	//Set the mask on Pin change interrupt 1 so that only PCINT12 (PC4) triggers
	//the interrupt. see doc8161.pdf Rev. 8161D – 10/09, ch 12.2.1
	PCMSK1 |= (1<<PCINT12);
	return;
}

void hcsr04_init(void){
	state = ST_IDLE;
}

int hcsr04_send_pulse(void){
	if (state != ST_IDLE)
		return 0;

	pin_init();
	PORTC |= (1<<PC5);     // start initiate pulse
	timer_init();

	state = ST_SENDING_START_PULSE;

	TCNT1 = 0;
	return 1;
}

uint16_t hcsr04_get_pulse_length(void){
    return resp_pulse_length;
}

