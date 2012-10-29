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

#ifndef CUBE_H_
#define CUBE_H_

#include <stdint.h>
#include "env.h"

// Total data in a buffer
#define GS_BUF_BYTES (LEDS_Z * BYTES_PER_LAYER)

/* Front and back buffers. Front is the one being drawn on and back is
 * the one that should be manipulated by effects. There are some
 * exceptions to this rule when doing some very nasty effects. */
extern uint8_t *gs_buf_front;
extern uint8_t *gs_buf_back;

/**
 * Swap buffers. Call this only from interrupt handlers or places
 * where no interrupts may occur.
 */
void gs_buf_swap(void);

/**
 * Restore buffers after NO_FLIP effect. May be safely run even if the
 * last effect was FLIP effect. Must be called when there is no
 * possibility of gs_buf_swap happening the same time.
 */
void gs_restore_bufs(void);

#endif /* CUBE_H_ */
