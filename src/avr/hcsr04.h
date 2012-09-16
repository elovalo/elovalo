/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
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

#ifndef HCSR04_H_
#define HCSR04_H_

/**
 * Device driver interface functions for HC-SR04 ultrasound module
 */

/**
 * Starts a new measurement cycle.
 * Returns 1 on success, 0 on failure
 * NOTE! This is for testing purposes, use hcsr04_start_continuous_meas() instead
 * TODO: to be removed
 */
int hcsr04_send_pulse(void);

/**
 * Starts the continuous measurement mode.
 * Returns 1 on success, 0 on failure
 */
int hcsr04_start_continuous_meas(void);

/**
 * Stops the continuous measurement mode.
 */
void hcsr04_stop_continuous_meas(void);

/**
 * Returns the result value of the latest measurement cycle,
 * or HCSR04_MEAS_FAIL on failure.
 */
#define HCSR04_MEAS_FAIL 0xFFFF
uint16_t hcsr04_get_pulse_length(void);

/**
 * Returns the result of the latest measurement cycle in cm,
 * or HCSR04_MEAS_FAIL on failure.
 */
uint16_t hcsr04_get_distance_in_cm(void);

#endif /* HCSR04_H_ */
