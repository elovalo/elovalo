/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ADC_H_
#define ADC_H_

/**
 * Device driver interface functions for ATmega328P ADC
 * See doc8161.pdf Rev. 8161D – 10/09, ch 23
 */

#if 0
/**
 * Initialize ADC channels
 * Required only if adc_read() is used.
 */
void adc_init();

/**
 * Starts a conversion cycle and waits for it to complete
 * For testing purposes only.
 */
uint16_t adc_read(uint8_t ch);
#endif

/**
 * Returns the latest conversion result for a given channel
 */
uint16_t adc_get(uint8_t channel);

/**
 * Starts a continuous conversion.
 * Results can be read with adc_get().
 */
void adc_start(void);

/**
 * Stops the continuous conversion which was started with adc_start().
 */
void adc_stop(void);

#endif /* ADC_H_ */
