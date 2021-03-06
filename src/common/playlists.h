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

/* The C file for this header is automatically generated and located
 * at ../generated/playlists.c */

typedef struct {
	uint8_t id;
	uint16_t length;
	const void *data;
} playlistitem_t;

extern const playlistitem_t master_playlist[];
extern const uint8_t master_playlist_len;
extern const uint8_t playlists[];
extern const uint8_t playlists_len;
extern const char playlists_json[];
extern const uint16_t playlists_json_len;
