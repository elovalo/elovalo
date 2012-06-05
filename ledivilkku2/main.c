/**
 * test.c
 */

#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/delay.h>

#include "init.h"
#include "main.h"
//#include "ym2612.h"
//#include "input.h"

//int ym2612_data=0x00;
//int input_states=0xF0;
//int *ptr_input_states = &input_states;
//int i;
volatile uint8_t producer;
volatile uint8_t consumer;

volatile uint8_t RXpuskuri;
volatile uint8_t TXpuskuri[8];
volatile uint8_t sendData=0;
volatile uint8_t i=0;
volatile char c=0; //testing variable...

volatile uint8_t spiBufferCounter = 0; //Counter for SPI buffer

#define SPI_BUF_SIZE 64

uint8_t spiReceiveBuffer[SPI_BUF_SIZE]; //SPI buffer for receiving
uint8_t spiTransmitBuffer[SPI_BUF_SIZE]; //SPI buffer for receiving

#define BAUD_RATE 9600UL

struct Usartpuskuri {
	//Usartpuskuri *next;
	uint8_t value;
};

int main() {

	initPorts();
	initUSART();
	initSPI();
	sei();

	while(1){

		//for(uint8_t j=0;j<8; j++){
		//	SPI_MasterTransmit(TXpuskuri[j]);
		//}

		//SPI_Transfer(c);
		//UDR0=c;
		_delay_ms(10);
		c++;
//		if(sendData==1){
//			for(int i=0;i<255;i++){
//
//				while(!(UCSR0A & (1<<UDRE0))){ //Odota, että lähetyspuskuri tyhjä.
//				}
//
//				UDR0 = i;
//			}
//			sendData=0;
//		}
	}

	return 0;
}

//Send byte via USART
void USART_Transmit()
{

}

//SPI master transmission
//Return slave data.
uint8_t SPI_Transfer(char cData)
{
	/* Start transmission */
	PORTB |= (1<<PB2);
	SPDR = cData; //Send byte
	/* Wait for transmission complete */
	while(!(SPSR & (1<<SPIF)));
	PORTB &= ~(1<<PB2);

	return SPDR;
}

//SPI transmit interrupt vector
//ISR(SPI_STC_vect)
//{
//  spiReceiveBuffer[spiBufferCounter++] = received_from_spi(0x00);
//  spiTransmitBuffer[spiBufferCounter];
//}

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
