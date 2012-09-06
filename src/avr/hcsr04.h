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
