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

/**
 * Functions for program space manipulation. When not built for AVR
 * environment (like building to x86), use stubs which skip program
 * space manipulation.
 */

#ifdef AVR
// On AVR use the provided library and define some helpers, too.

#include <avr/pgmspace.h>

/* Usage example: (init_t)pgm_get(effects[3].init,word); For more
   information about type parameter, see avr-libc user manual about
   pgmspace. */
#define pgm_get(var,type)			\
  pgm_read_ ## type ## _near(&(var))

#else
// On other platforms, implement some dummy macros

#define PROGMEM
#define PGM_P const char *

#define pgm_get(var,type) var

#endif
