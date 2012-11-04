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

/* Target specific serial helper function API */

/**
 * Processes a serial command if one is available.
 */
void process_serial(void);

/**
 * Sleeps if no serial traffic is available. This function does not
 * guarantee that serial traffic is available on return.
 */
void sleep_if_no_traffic(void);

/**
 * Reports to serial port that we experienced a (re-)boot.
 */
void serial_boot_report(void);
