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

/* It is assumed that X and Y form one layer on the LED cube. The
 * actual LED count may be smaller, of course.
 * 
 * SHIFT_REGISTER_BYTES tells how many bytes there are in shift
 * register of Z layer switcher.
 * 
 * If you are changing LED count or GS_DEPTH, make sure you have
 * compatible implementation of set_led() in effects/lib/utils.c */

#define LEDS_X 8
#define LEDS_Y 8
#define LEDS_Z 8
#define GS_DEPTH 12
#define BYTES_PER_LAYER 96
#define SHIFT_REGISTER_BYTES 1

/* To mirror cube coordinates, uncomment the axis. To rotate the cube
 * for 180 degrees, uncomment the both. */
//#define MIRROR_X
//#define MIRROR_Y
