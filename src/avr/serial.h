/* Functions for USART access */

#include <stdint.h>

#define TX_BUF_SIZE 8
#define RX_BUF_SIZE 64
#define TXRX_OK 0
#define TXRX_OVERFLOW 1

/* Receiver and transmitter states. TXRX_OK is normal value. */
extern volatile uint8_t tx_state;
extern volatile uint8_t rx_state;

void serial_send(uint8_t data);
uint8_t serial_available(void);
uint8_t serial_send_available(void);
void serial_RX_empty(void);
void serial_TX_empty(void);
uint8_t serial_read(void);
