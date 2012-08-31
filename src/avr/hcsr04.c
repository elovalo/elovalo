
//#define F_CPU 14745600

#include <stdio.h>
#include <math.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <inttypes.h>

volatile uint8_t pulse_flag, timing_flag;
volatile uint32_t pulse_count, pulse_time;

//TODO: hmm, elovalo has a common init.c, move this into it
void timer_init(void){
	// Set up interrupt
	TCCR0A |= (1<<WGM01);  // sets compare and reset to "top" mode
	TCCR0B |= (1<<CS00);   // set clock divider to 1
	//OCR0A = 74;            // set "top" for 5 usec
	OCR0A = 148;            // set "top" for 10 usec
	TIMSK0 |= (1<<OCIE0A); // turn interrupt on

	pulse_flag = 0;
	timing_flag = 0;
}

ISR(TIMER0_COMPA_vect) {

	if (pulse_flag == 1){
		PORTC &= ~(1<<PC5);    // stop pulse
		pulse_flag = 0;
		pulse_count = 0;
	}
	else{
		pulse_count++;
		timing_flag = 1;
	}
}
ISR(PCINT1_vect){
	if (timing_flag == 1){
		pulse_time = pulse_count;
		timing_flag = 0;
	}
}

//TODO: hmm, elovalo has a common init.c, move this into it
void pin_init(void){
	DDRC |= (1<<PC5); //output
	DDRC &= ~(1<<PC4); //input
	PORTC |= (1<<PC4);
	//Enable PIN Change Interrupt 1 - This enables interrupts on pins
	//PCINT14...8 see p70 of datasheet
	PCICR |= (1<<PCIE1);

	//Set the mask on Pin change interrupt 1 so that only PCINT12 (PC4) triggers
	//the interrupt. see p71 of datasheet
	PCMSK1 |= (1<<PCINT12);
	return;
}

void hcsr04_init(void){
	timer_init();
	pin_init();
}

void hcsr04_send_pulse(void){
	PORTC |= (1<<PC5);     // start pulse
	pulse_flag = 1;
	TCNT0 = 0;
}

uint32_t hcsr04_get_pulse_time(void){
    return pulse_time;
}

