/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/io.h>
#include <util/atomic.h>
#include <stdlib.h>

// number of successive channels in use from ADC0 upwards
#define CHANNEL_COUNT 2
#define DIDR0_MASK ((1 << CHANNEL_COUNT) - 1)

// prescaler of 128
// 16000000/128 = 125000 Hz
#define ADC_PRESCALER (_BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0))

// ADCL must be read first, see doc8161.pdf Rev. 8161D – 10/09, ch 23.2
#define READ_ADC_RESULT (ADCL | (ADCH<<8))

uint8_t curr_channel = 0;
uint16_t latest_conv_results[CHANNEL_COUNT];

void adc_init()
{
	// disable digital inputs of ADC0 ... ADCn
	// see e.g. http://www.openmusiclabs.com/learning/digital/atmega-adc/
	DIDR0 = DIDR0_MASK;

	// AREF = AVcc
	ADMUX = (1 << REFS0);

	// Enable ADC and set prescaler
	ADCSRA = _BV(ADEN) | ADC_PRESCALER;
}

#if 0
uint16_t adc_read(uint8_t ch)
{
	// select the corresponding channel 0~7
	// ANDing with '7' will always keep the value
	// of 'ch' between 0 and 7
	ch &= 0 b00000111;	// AND operation with 7
	ADMUX = (ADMUX & 0xF8) | ch;	// clears the bottom 3 bits before ORing

	// start single convertion
	// write '1' to ADSC
	ADCSRA |= (1 << ADSC);

	// wait for conversion to complete
	// ADSC becomes '0' again
	// till then, run loop continuously
	while (ADCSRA & (1 << ADSC)) ;

	return READ_ADC_RESULT;
}
#endif

uint16_t adc_get(uint8_t channel)
{
	uint16_t ret_val;

	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		ret_val = latest_conv_results[channel];
	}
	return ret_val;
}

void adc_start(void)
{
	adc_init();

	curr_channel = 0;
	ADCSRA |= _BV(ADSC) | _BV(ADIE);
}

void adc_stop(void)
{
	ADCSRA &= ~(_BV(ADSC) | _BV(ADIE));
}

ISR(ADC_vect)
{
	latest_conv_results[curr_channel] = READ_ADC_RESULT;

	curr_channel++;
	if (curr_channel >= CHANNEL_COUNT)
		curr_channel = 0;

	ADMUX = (ADMUX & 0xe0) | curr_channel;

	//start the next conversion
	ADCSRA |= _BV(ADSC);
}
