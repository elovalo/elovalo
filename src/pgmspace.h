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

#endif
