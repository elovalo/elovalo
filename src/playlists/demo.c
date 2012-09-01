#include <stdint.h>
#include "../pgmspace.h"
#include "playlist.h"

PROGMEM const char s_sine[] = "sine";
PROGMEM const char s_wave[] = "wave";

const playlistitem_t playlist[] PROGMEM = {
	{ s_sine, 1000 },
	{ s_wave, 1000 }
};

const uint8_t playlist_len = sizeof(playlist) / sizeof(playlistitem_t);
