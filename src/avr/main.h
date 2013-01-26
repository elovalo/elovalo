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

#include "../common/effects.h"

// Operating modes
#define MODE_IDLE           0x00 // Do not update display buffers
#define MODE_EFFECT         0x01 // Draw effect
#define MODE_PLAYLIST       0x02 // Playlist
#define MODE_SLEEP          0x03 // Same as idle, but cube must be started first

extern uint8_t mode; // If you need to change the running effeet

void select_playlist_item(uint8_t index);
void init_current_effect(void);
uint8_t change_current_effect(uint8_t i);
uint8_t change_playlist(uint8_t i);

void use_stored_effect(void);
void use_stored_playlist(void);

uint8_t get_mode(void);
void set_mode(uint8_t m);

// Some functions to alter AVR_ZCL states
void reset_modified_state(void);
void mark_playlist_modified(void);
void mark_effect_modified(void);
void mark_text_modified(void);

extern uint8_t active_effect; // Index of the active effect. Used for playlist
extern uint8_t active_playlist; // Index of the active playlist
