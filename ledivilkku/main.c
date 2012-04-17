/*
 * test.c
 */

#include <util/delay.h>
#include <avr/io.h>
//#include "ym2612.h"
//#include "input.h"
#include "main.h"

//int ym2612_data=0x00;
//int input_states=0xF0;
//int *ptr_input_states = &input_states;
//int i;

int main() {
	//YM_CS_ON; //Chip not selected

	initPorts();
	//initYM2612();
	//readWriteFromYM2612();

	while(1){
		//Read MIDI
		_delay_ms(1500);
		PORTB=0x00;
		_delay_ms(1000);
		PORTB=0xff;
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

	/**Port A initialization
	*Func7=In Func6=In Func5=In Func4=In Func3=In Func2=In Func1=In Func0=In
	*State7=T State6=T State5=T State4=T State3=T State2=T State1=T State0=T
	*/
	PORTB=0xff;
	DDRB=0b00000000;

	/** Port B initialization
	* Func7=In Func6=In Func5=In Func4=In Func3=In Func2=In Func1=In Func0=In
	* State7=T State6=T State5=T State4=T State3=T State2=T State1=T State0=T
	*/
	//PORTB=0x00;
	//DDRB=0xff;

	/** Port C initialization
	*	Func7=In Func6=In Func5=In Func4=In Func3=In Func2=In Func1=In Func0=In
	* 	State7=T State6=T State5=T State4=T State3=T State2=T State1=T State0=T
	*/
	//PORTC=0x00;
	//DDRC=0x00;

	/** Port D initialization
	* Func7=Out Func6=Out Func5=Out Func4=Out Func3=Out Func2=Out Func1=Out Func0=Out
	* State7=0 State6=0 State5=0 State4=0 State3=0 State2=0 State1=0 State0=0
	*/
	//PORTD=0x00;
	//DDRD=0xFF;

	/* 	Port E initialization
	* 	Func7=Out Func6=Out Func5=Out Func4=Out Func3=Out Func2=Out Func1=In Func0=In
	* 	State7=0 State6=0 State5=0 State4=0 State3=0 State2=0 State1=T State0=T
	*/
	//PORTE=0b00011001;
	//DDRE=0xFF;

	/**	Port F initialization
	*	Func7=In Func6=In Func5=In Func4=In Func3=In Func2=In Func1=In Func0=In
	*	State7=T State6=T State5=T State4=T State3=T State2=T State1=T State0=T
	*/
	//PORTF=0x00;
	//DDRF=0x00;

	/* 	Port G initialization
	*	Func4=In Func3=In Func2=In Func1=In Func0=In
	*	State4=T State3=T State2=T State1=T State0=T
	*/
	//PORTG=0x00;
	//DDRG=0x00;

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

	__asm ("cli");

}

/*void initYM2612(){
	YM_WR_ON;
	YM_RD_ON;
	YM_A0_OFF;
	YM_A1_OFF;

	YM_CS_ON;*/

	//Reset
//	_delay_ms(10);
//	PORTE&=~(1<<4);
//	_delay_ms(20);
//	PORTE|=(1<<4);
//
//	//Start initializing registers
//
//	writeToPart0(0x22, 0x00);
//
//	//isReady(0x22, &ym2612_data);
//
//	//PORTB = ym2612_data;
//
//	writeToPart0(0x28, 0x00);
//	writeToPart0(0x28, 0x01);
//	writeToPart0(0x28, 0x02);
//	writeToPart0(0x28, 0x03);
//	writeToPart0(0x28, 0x04);
//	writeToPart0(0x28, 0x05);
//	writeToPart0(0x28, 0x06);
//
//	/*keyOnOff(0,0);
//	keyOnOff(1,0);
//	keyOnOff(2,0);
//	keyOnOff(4,0);
//	keyOnOff(5,0);
//	keyOnOff(6,0);*/
//
//	allChannelOff();
//
//	writeToPart0(0x2B, 0x00);
//	disableDAC();
//
//	//DT1/MUL
//	writeToPart0(0x30, 0x71);
//
//	writeToPart0(0x34, 0x0D);
//
//	writeToPart0(0x38, 0x33);
//
//	writeToPart0(0x3C, 0x01);
//
//
//	//Total Level
//	writeToPart0(0x40, 0x23);
//
//	writeToPart0(0x44, 0x2D);
//
//	writeToPart0(0x48, 0x26);
//
//	writeToPart0(0x4C, 0x00);
//
//	//RS/AR
//
//	writeToPart0(0x50, 0x5F);
//
//	writeToPart0(0x54, 0x99);
//
//	writeToPart0(0x58, 0x5F);
//
//	writeToPart0(0x5C, 0x94);
//
//	//AM/D1R
//
//	writeToPart0(0x60, 0x05);
//
//	writeToPart0(0x64, 0x05);
//
//	writeToPart0(0x68, 0x05);
//
//	writeToPart0(0x6C, 0x07);
//
//	//D2R
//
//	writeToPart0(0x70, 0x02);
//
//	writeToPart0(0x74, 0x02);
//
//	writeToPart0(0x78, 0x02);
//
//	writeToPart0(0x7C, 0x02);
//
//	//D1L/RR
//
//	writeToPart0(0x80, 0x11);
//
//	writeToPart0(0x84, 0x11);
//
//	writeToPart0(0x88, 0x11);
//
//	writeToPart0(0x8C, 0xA6);
//
//	//Proprietary
//
//	writeToPart0(0x90, 0x00);
//
//	writeToPart0(0x94, 0x00);
//
//	writeToPart0(0x98, 0x00);
//
//	writeToPart0(0x9C, 0x00);
//
//	//Feedback/algorithm
//
//	writeToPart0(0xB0, 0x32);
//
//	//Both speakers on
//
//	writeToPart0(0xB4, 0xC0);
//
//	//Key off
//
//	writeToPart0(0x28, 0x00);
//
//	//Set Frequency
//	writeToPart0(0xA4, 0x22);
//
//	writeToPart0(0xA0, 0x69);
//
//	_delay_ms(500);
//	//Key on
//	//PORTB=0xf0;
//
//	writeToPart0(0x28, 0xF0);
//
//	//_delay_ms(2500);
//	//writeToPart0(0x28, 0xF0);

//}
