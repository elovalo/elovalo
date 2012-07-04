/**
 * Ledikuutio Main...
 * TODO: 	Make sure that SRAM usage stays as low as possible,
 * 			avoid using local variables or declaring variables at runtime.
 *
 * TODO: 	Check if there's way to monitor the stack usage at runtime
 * 			to avoid stack overflow when pushing close the SRAM limit...
 *
 */

#include <avr/interrupt.h>
#include <avr/io.h>
//#include <util/delay.h>
#include "pinMacros.h"
#include "main.h"
#include "init.h"
#include "tlc5940.h"

//int input_states=0xF0;
//int *ptr_input_states = &input_states;
//int i;
//volatile uint8_t producer;
//volatile uint8_t consumer;

uint8_t c=0; //Global testing variable...

//Grayscale data array, lenght is 24 * number of devices * number of layers in a cube...
//TODO: currently fixed to 1 device, calculate from number of devices an fill with values
uint8_t GSdata[24*TLC5940]={0xff,0xf0,0x00,0xff,0xf0,0x00,0xff,0xf0,
							0x00,0xff,0xf0,0x00,0xff,0xf0,0x00,0xff,
							0xf0,0x00,0xff,0xf0,0x00,0xff,0xf0,0x00};

//TODO: backbuffer for double buffering...
//volatile uint8_t *BackBuffer = GSdata2;

//Pointer to the GS data buffer that holds the data to be sent to the TLC5940
uint8_t *FrontBuffer = GSdata;

//USART variables...
//volatile uint8_t RXpuskuri;
//volatile uint8_t TXpuskuri[8];
//volatile uint8_t sendData=0;
//volatile uint8_t i=0;

int main() {

	cli();
	disableWDT();
	initPorts();
	initSPI();

	initTLC5940();
	initBLANKTimer();

	sei();

	InitGScycle(); //TODO: Send first byte to the SPI bus...

	/* TODO: flipping for the buffers...
	* flipitiflip bufferille
	* *FrontBuffer ^= *BackBuffer;
	* *BackBuffer ^= *FrontBuffer;
	* *FrontBuffer ^= *BackBuffer;
	*/

	while(1){
	}

	return 0;
}

////Send byte via USART
//void USART_Transmit()
//{
//
//}
//
////Generic SPI master transmission
////Return slave data.
//void SPI_Transfer(uint8_t cData)
//{
//	/* Start transmission */
//	PORTB |= (1<<PB1);
//	SPDR = cData; //Send byte
//	/* Wait for transmission complete */
//	while(!(SPSR & (1<<SPIF)));
//	PORTB &= ~(1<<PB1);
//}

////SPI Transfer for TLC5940
//void SPI_Transfer_TLC5940(uint8_t FrontBuffer[])
//{
//	GSdataCounter=0;
//	/* Start transmission */
//	PORTB &= ~(1<<PB1);
//
//	//notTransferring=0; //we're transferring data...
//	SPDR = FrontBuffer[0]; //Send first byte to initialize the ISR managed transfer
//
//	/* Wait for transmission complete */
//	//while(!(SPSR & (1<<SPIF)));
//	//PORTB &= ~(1<<PB2);
//}

////USART Received byte vector
//ISR(USART_RX_vect)
//{
//	TXpuskuri[i]=UDR0;
//	if(i>=8){
//		i=0;
//	}
//	UDR0 = TXpuskuri[i];
//	i++;
//	//SPI_MasterTransmit(TXpuskuri);
//}

//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{

	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
