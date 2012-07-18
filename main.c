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

volatile uint16_t c=0; //Global testing variable...

//Grayscale data array, lenght is 24 * number of devices * number of layers in a cube...
//TODO: currently fixed to 1 device, calculate from number of devices an fill with values
//TODO: remember to clear these arrays or initialize them to 0 before blank goes low.
uint8_t GSdata[24*TLC5940]={0x00};

uint8_t GSdata2[24*TLC5940]={0x00};

uint8_t *Midbuffer = GSdata2; //TODO: arrange the code more logically, this should be moved...

//TODO: backbuffer for double buffering...
uint8_t *BackBuffer = GSdata2;

//Pointer to the GS data buffer that holds the data to be sent to the TLC5940
uint8_t *FrontBuffer = GSdata;

//USART variables...
#define BUF_SIZE 64
#define RX_OK 0
#define RX_OVERFLOW 1
uint8_t rx_buf[BUF_SIZE];
uint8_t * volatile rx_in_p;
uint8_t * volatile rx_out_p;

volatile uint8_t rx_state = RX_OK;

int main() {

	cli();

	disableWDT();
	initPorts();
	initSPI();

	initTLC5940();
	initBLANKTimer();

	// RX ring buffer setup
	rx_in_p = rx_buf;
	rx_out_p = rx_buf;

	initUSART();
	sei();

	InitGScycle(); //TODO: Send first byte to the SPI bus...

	while(1){

//		if (isAfterFlip) {
//		}

		if(serial_available()>0){
			pin_toggle(DEBUG_LED);
			USART_Transmit(serial_read());
		}

	}

	return 0;
}

void clearArray(volatile uint8_t *arr, uint8_t len) {

	for (uint8_t r = 0; r < len; r++) {
		arr[r] = 0x00;
	}

}

//Send byte via USART
void USART_Transmit(uint8_t data)
{
	/* Wait for empty transmit buffer*/
	while( !( UCSR0A & (1<<UDRE0)) );
	UDR0 = data;
	/* Put data into buffer, sends the data*/

}

//USART Received byte vector
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

//USART Received byte vector
ISR(USART_RX_vect)
{
	*rx_in_p++ = UDR0;

	// Wrap to start
	if (rx_in_p == rx_buf + BUF_SIZE) rx_in_p = rx_buf;
	
	if (rx_in_p == rx_out_p) {
		// Overflow condition
		rx_state = RX_OVERFLOW;
	}
}

//USART Transmit complete interrupt
ISR(USART_TX_vect)
{
	//TODO: Implement usart interrupt driven transmission
	//Get next byte from TX buffer and put it to USART transmit buffer
	//Manage buffer
}

/**
 * Returns the number of bytes available in receive buffer
 */
uint8_t serial_available(void) {
	return (rx_in_p - rx_out_p) % BUF_SIZE; //TODO: Ensure pointer promotion to signed for possible negative result...
}

/**
 * Empties receive buffer.
 */
void serial_empty(void) {
	rx_out_p = rx_in_p;
}

/**
 * Reads a byte from receive buffer. Does not check underrun
 * condition; the user is responsible to check serial_available()
 * before calling this.
 */
uint8_t serial_read(void) {
	uint8_t data = *rx_out_p++;
	if (rx_out_p == rx_buf + BUF_SIZE) rx_out_p = rx_buf;
	return data;
}

//If an interrupt happens and there isn't an interrupt handler, we go here!
ISR(BADISR_vect)
{
	pin_high(DEBUG_LED); //Give us an indication about an error condition...
	while(1){
		}
}
