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

#include "cube.h"

// Define the buffers for LED cube grayscale data
uint8_t gs_buf_a[GS_BUF_BYTES]={0x00};
uint8_t gs_buf_b[GS_BUF_BYTES]={0x00};

uint8_t *gs_buf_front = gs_buf_a;
uint8_t *gs_buf_back = gs_buf_b;

void gs_buf_swap(void) {
	uint8_t *tmp = gs_buf_front;
	gs_buf_front = gs_buf_back;
	gs_buf_back = tmp;
}

void gs_restore_bufs(void) {
	if (gs_buf_front == gs_buf_a) {
		gs_buf_back = gs_buf_b;
	} else {
		gs_buf_back = gs_buf_a;
	}
}

// Some sanity checks
#if (LEDS_X * LEDS_Y * GS_DEPTH) > (8 * BYTES_PER_LAYER)
#error "There are more LED data on X-Y layer than there is BYTES_PER_LAYER"
#endif

#if (1 << (8*SHIFT_REGISTER_BYTES)) < LEDS_Z
#error "LEDS_Z is too large; does not fit inside SHIFT_REGISTER_BYTES"
#endif
