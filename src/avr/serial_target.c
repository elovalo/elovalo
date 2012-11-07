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

#if defined SIMU

/* Simulation target has no serial port nor sleep, so these functions
 * are no-ops */
void process_serial(void) {}
void sleep_if_no_traffic(void) {}
void serial_boot_report(void) {}

#elif defined AVR_ZCL

#include <avr/sleep.h>
#include "serial.h"
#include "zcl_skeleton.h"

/* process_serial() is implemented in zcl_skeleton to keep the serial
   mess out of here */

void sleep_if_no_traffic(void)
{
	if (zcl_receiver_has_data()) return;
	sleep_mode();
}

void serial_boot_report(void) {
	// TODO implement sending DeviceConfigurationEP.restartReason
}

#elif defined AVR_ELO

#include <avr/sleep.h>
#include "serial.h"
#include "serial_elo.h"

void process_serial(void)
{
	if (!serial_available()) return;
	uint8_t cmd = serial_read();			
	serial_elo_process(cmd);
}

void sleep_if_no_traffic(void)
{
	if (serial_available()) return;
	sleep_mode();
}

void serial_boot_report(void) {
	serial_elo_init();
}

#else

#error Unsupported serial communication type

#endif
