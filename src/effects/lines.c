#include "common.h"

void effect(void) {
	if(!(ticks % 10))
		line(randint(0, 7), randint(0, 7), randint(0, 7),
			randint(0, 7), randint(0, 7), randint(0, 7),
			MAX_INTENSITY);
}
