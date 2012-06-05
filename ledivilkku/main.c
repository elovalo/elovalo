/*
 * test.c
 */

#include <avr/io.h>
#include <util/delay.h>
//#include "init.h"
#include "main.h"
#include <avr/interrupt.h>
//#include "ym2612.h"
//#include "input.h"


//int ym2612_data=0x00;
//int input_states=0xF0;
//int *ptr_input_states = &input_states;
//int i;



ISR(USART_RXC_vectr)
{
	_delay_ms(1500);
	PORTB=0x00;
	_delay_ms(1000);
	PORTB=0xff;
}

int main() {
	//YM_CS_ON; //Chip not selected

	initPorts();
	//initYM2612();
	//readWriteFromYM2612();

	while(1){
		//Read MIDI
		//Read DIN Module
		//input_states = readInputs();
		//PORTB=input_states;

		//Analyze MIDI messages received

		//Change the YM2612 Registers accordingly to midi and DIN.

		//Display LCD
	}

return 0;
}

void initPorts(){

	/**
	* Port B initialization
	* PORTB Output all
	* Set to zero
	*/

	PORTB=0xff;
	DDRB=0b00000000;

	/** Port C initialization*/

	PORTC=0x00;
	DDRC=0xff;

	U2X0=0; //Normal Mode
	UCSR0A=0x00;
	UCSR0B=0b10010000;
	UCSR0C=0b00000110;

	// USART0 Baud rate: 9600
	UBRR0H=0x00;
	UBRR0L=0b1000100;

	__asm ("cli");

	/** Timer/Counter 0 initialization
	* 	Clock source: System Clock
	* 	Clock value: Timer 0 Stopped
	* 	Mode: Normal top=FFh
	*	OC0 output: Disconnected
	*/
	//ASSR=0x00;
	//TCCR0=0x00;
	//TCNT0=0x00;
	//OCR0=0x00;

	/* Timer/Counter 1 initialization
	* Clock source: System Clock
	* Clock value: Timer 1 Stopped
	* Mode: Normal top=FFFFh
	* OC1A output: Discon.
	* OC1B output: Discon.
	* OC1C output: Discon.
	* Noise Canceler: Off
	* Input Capture on Falling Edge
	* Timer 1 Overflow Interrupt: Off
	* Input Capture Interrupt: Off
	* Compare A Match Interrupt: Off
	* Compare B Match Interrupt: Off
	* Compare C Match Interrupt: Off
	*/
	//TCCR1A=0x00;
	//TCCR1B=0x00;
	//TCNT1H=0x00;
	//TCNT1L=0x00;
	//ICR1H=0x00;
	//ICR1L=0x00;
	//OCR1AH=0x00;
	//OCR1AL=0x00;
	//OCR1BH=0x00;
	//OCR1BL=0x00;
	//OCR1CH=0x00;
	//OCR1CL=0x00;

	/* Timer/Counter 2 initialization
	* Clock source: System Clock
	* Clock value: Timer 2 Stopped
	* Mode: Normal top=FFh
	* OC2 output: Disconnected
	*/
	//TCCR2=0x00;
	//TCNT2=0x00;
	//OCR2=0x00;

	/* Timer/Counter 3 initialization
	* Clock source: System Clock
	* Clock value: Timer 3 Stopped
	* Mode: Normal top=FFFFh
	* Noise Canceler: Off
	* Input Capture on Falling Edge
	* OC3A output: Discon.
	* OC3B output: Discon.
	* OC3C output: Discon.
	* Timer 3 Overflow Interrupt: Off
	* Input Capture Interrupt: Off
	* Compare A Match Interrupt: Off
	* Compare B Match Interrupt: Off
	* Compare C Match Interrupt: Off
	*/
	//TCCR3A=0x00;
	//TCCR3B=0x00;
	//TCNT3H=0x00;
	//TCNT3L=0x00;
	//ICR3H=0x00;
	//ICR3L=0x00;
	//OCR3AH=0x00;
	//OCR3AL=0x00;
	//OCR3BH=0x00;
	//OCR3BL=0x00;
	//OCR3CH=0x00;
	//OCR3CL=0x00;

	/** External Interrupt(s) initialization
	* INT0: Off
	* INT1: Off
	* INT2: Off
	* INT3: Off
	* INT4: Off
	* INT5: Off
	* INT6: Off
	* INT7: Off
	*/
	//EICRA=0x00;
	//EICRB=0x00;
	//EIMSK=0x00;

	// Timer(s)/Counter(s) Interrupt(s) initialization
	//TIMSK=0x00;
	//ETIMSK=0x00;

	/** Analog Comparator initialization
	* Analog Comparator: Off
	* Analog Comparator Input Capture by Timer/Counter 1: Off
	*/
	//ACSR=0x80;
	//SFIOR=0x00;

	// USART0 initialization
	// Communication Parameters: 8 Data, 1 Stop, No Parity
	// USART0 Receiver: On
	// USART0 Transmitter: Off
	// USART0 Mode: Asynchronous
	// USART0 Baud rate: 31250

}
