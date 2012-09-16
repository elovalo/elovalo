/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Returns the time in centiseconds (not really, 0.008 seconds after
 * clock granularity change). TODO rename this function. */
uint16_t centisecs(void);

void reset_time(void);

/* Time functions are modelled after POSIX */
typedef uint32_t time_t;

/**
 * Returns POSIX time. If the clock is not running (never set by
 * stime) it returns 0. (this part doesn't conform POSIX). Do NOT call
 * this from interrupts because this turns toggles interrupts.
 */
time_t time(time_t *t);

/**
 * Sets POSIX time to this device. Always succeedes and returns 0. Do
 * NOT call this from interrupts because this turns toggles
 * interrupts.
 */
int stime(time_t *t);

/**
 * This returns the time WITHOUT any validation. This is quick but
 * MUST be called only with certainty that the time is okay and with
 * interrupts disabled.
 */
time_t unsafe_time(time_t *t);
