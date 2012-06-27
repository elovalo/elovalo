/**
 * Ledikuutio Main...
 */

#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/delay.h>

#include "pinMacros.h"
#include "main.h"
#include "init.h"
#include "tlc5940.h"

//int input_states=0xF0;
//int *ptr_input_states = &input_states;
//int i;
volatile uint8_t producer;
volatile uint8_t consumer;

volatile uint8_t RXpuskuri;
volatile uint8_t TXpuskuri[8];
volatile uint8_t sendData=0;
volatile uint8_t i=0;

uint8_t c=0; //testing variable...

//Display GS data and buffers
uint8_t GSdata[2] = {0b00001011, 0b10101101};
uint8_t *FrontBuffer = GSdata;

//volatile uint8_t *BackBuffer = GSdata2;
volatile uint8_t GSdataCounter=0;

volatile uint8_t spiBufferCounter = 0; //Counter for SPI buffer


uint8_t FirstCycle = 0;


//#define SPI_BUF_SIZE 64

//#define BAUD_RATE 9600UL

int main() {

	initPorts();
	//initUSART();
	initSPI();

	sei();

	//flipitiflip bufferille
	//*FrontBuffer ^= *BackBuffer;
	//*BackBuffer ^= *FrontBuffer;
	//*FrontBuffer ^= *BackBuffer;

	//SPI_Transfer_TLC5940(FrontBuffer);
	//SPI_Transfer(GSdata[0]);

	while(1){
		_delay_ms(500);
		pin_high(DEBUG_LED);
		_delay_ms(500);
		pin_low(DEBUG_LED);
	}
		//if(notTransferring){
		//SPI_Transfer_TLC5940(GSdata);
		//}
		//SPI_Transfer(c);

		//UDR0=c;
		//_delay_ms(100);
//		if(sendData==1){
//			for(int i=0;i<255;i++){
//
//				while(!(UCSR0A & (1<<UDRE0))){ //Odota, ett� l�hetyspuskuri tyhj�.
//				}
//
//				UDR0 = i;
//			}
//			sendData=0;
//		}
	return 0;
}

//Send byte via USART
void USART_Transmit()
{

}

//Generic SPI master transmission
//Return slave data.
void SPI_Transfer(uint8_t cData)
{
	/* Start transmission */
	PORTB |= (1<<PB1);
	SPDR = cData; //Send byte
	/* Wait for transmission complete */
	while(!(SPSR & (1<<SPIF)));
	PORTB &= ~(1<<PB1);
}

//SPI Transfer for TLC5940
void SPI_Transfer_TLC5940(uint8_t FrontBuffer[])
{
	GSdataCounter=0;
	/* Start transmission */
	PORTB &= ~(1<<PB1);

	//notTransferring=0; //we're transferring data...
	SPDR = FrontBuffer[0]; //Send first byte to initialize the ISR managed transfer

	/* Wait for transmission complete */
	//while(!(SPSR & (1<<SPIF)));
	//PORTB &= ~(1<<PB2);
}

//SPI transmit interrupt vector
ISR(SPI_STC_vect)
{
	//_delay_ms(500);
	//If theres data to be sent...
	GSdataCounter++;
	if(GSdataCounter<=GS_DATA_LENGHT){
		SPDR = FrontBuffer[GSdataCounter];
	}
}

//USART Received byte vector
ISR(USART_RX_vect)
{
	TXpuskuri[i]=UDR0;
	if(i>=8){
		i=0;
	}
	UDR0 = TXpuskuri[i];
	i++;
	//SPI_MasterTransmit(TXpuskuri);
}

ISR(TIMER2_COMPA_vect)
{
	pin_high(BLANK);
	pin_high(XLAT);
	pin_low(XLAT);
	if(FirstCycle){
		pin_high(SCLK);
		pin_low(SCLK);
		FirstCycle = 0;
	}
   // Code to execute on ISR fire here
   InitGScycle();
}
