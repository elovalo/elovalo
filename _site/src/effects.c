/* GENERATED FILE! DON'T MODIFY!!!
 * Led cube effects
 */

#include <stdlib.h>
#include <stdint.h>
#include "pgmspace.h"
#include "env.h"
#include "effects.h"
#include "effects/common.h"

static void init_stairs_y(void);
static void init_matrix(void);
static void init_heart(void);
static void init_finite_worm(void);
static void init_rain(void);
static void init_stairs_x(void);
static void init_template(void);
static void init_game_of_life(void);
static void init_worm(void);
static void init_cube(void);
static void init_brownian(void);
static void init_countdown(void);
static void init_line(void);
static void init_starfield(void);
static void init_particles(void);

static void effect_gradient(void);
static void effect_sine(void);
static void effect_matrix(void);
static void effect_heart(void);
static void effect_finite_worm(void);
static void effect_all_on(void);
static void effect_clock(void);
static void effect_tornado(void);
static void effect_lines(void);
static void effect_tube(void);
static void effect_rain(void);
static void effect_character(void);
static void effect_wave(void);
static void effect_circle(void);
static void effect_template(void);
static void effect_layers(void);
static void effect_constant(void);
static void effect_sphere(void);
static void effect_game_of_life(void);
static void effect_worm(void);
static void effect_brownian(void);
static void effect_wireframe(void);
static void effect_countdown(void);
static void effect_starfield(void);
static void effect_fish(void);
static void effect_test_sensor1(void);
static void effect_particles(void);
static void effect_scroll_text(void);

PROGMEM const char s_gradient[] = "gradient";
PROGMEM const char s_sine[] = "sine";
PROGMEM const char s_stairs_y[] = "stairs_y";
PROGMEM const char s_matrix[] = "matrix";
PROGMEM const char s_heart[] = "heart";
PROGMEM const char s_finite_worm[] = "finite_worm";
PROGMEM const char s_all_on[] = "all_on";
PROGMEM const char s_clock[] = "clock";
PROGMEM const char s_tornado[] = "tornado";
PROGMEM const char s_lines[] = "lines";
PROGMEM const char s_tube[] = "tube";
PROGMEM const char s_rain[] = "rain";
PROGMEM const char s_stairs_x[] = "stairs_x";
PROGMEM const char s_character[] = "character";
PROGMEM const char s_wave[] = "wave";
PROGMEM const char s_circle[] = "circle";
PROGMEM const char s_template[] = "template";
PROGMEM const char s_layers[] = "layers";
PROGMEM const char s_constant[] = "constant";
PROGMEM const char s_sphere[] = "sphere";
PROGMEM const char s_game_of_life[] = "game_of_life";
PROGMEM const char s_worm[] = "worm";
PROGMEM const char s_cube[] = "cube";
PROGMEM const char s_brownian[] = "brownian";
PROGMEM const char s_wireframe[] = "wireframe";
PROGMEM const char s_countdown[] = "countdown";
PROGMEM const char s_line[] = "line";
PROGMEM const char s_starfield[] = "starfield";
PROGMEM const char s_fish[] = "fish";
PROGMEM const char s_test_sensor1[] = "test_sensor1";
PROGMEM const char s_particles[] = "particles";
PROGMEM const char s_scroll_text[] = "scroll_text";






#define WORM_LENGTH 10


#define SECS_IN_DAY ((time_t)60*60*24)
#define TIMEZONE_SECS (3*60*60)














































static uint8_t get_amount_of_neighbours(uint8_t x, uint8_t y, uint8_t z);
static void set_leds(uint8_t x, uint8_t y, uint8_t z);









uint16_t hcsr04_get_distance_in_cm(void);




static union {
struct {
	xyz_t xyz[10];
} matrix;

struct {
	uint8_t y;
} heart;

struct {
	uint16_t pos[3];
	uint16_t dir;
	int8_t speed;
	uint16_t prev_dirs[WORM_LENGTH];
	int8_t prev_speeds[WORM_LENGTH];
	uint8_t prev_dir_i;
} finite_worm;

struct {
	uint16_t seed;
} rain;

struct {
	float avg;
} wave;

struct {
	uint8_t is_alive;
} game_of_life;

struct {
	uint16_t pos[3];
	uint16_t dir;
	int speed;
} worm;

struct {
	xyz_t xyz;
} brownian;

struct {
	uint8_t cur;
} countdown;

struct {
	xyz_t xyz[10];
} starfield;

struct {
	xyz_t xyz[5];
} particles;

} vars;

const effect_t effects[] PROGMEM = {
	{ s_gradient, NULL, &effect_gradient, FLIP, 4 },
	{ s_sine, NULL, &effect_sine, FLIP, 4 },
	{ s_stairs_y, &init_stairs_y, NULL, NO_FLIP, 4 },
	{ s_matrix, &init_matrix, &effect_matrix, FLIP, 4 },
	{ s_heart, &init_heart, &effect_heart, NO_FLIP, 5 },
	{ s_finite_worm, &init_finite_worm, &effect_finite_worm, NO_FLIP, 6 },
	{ s_all_on, NULL, &effect_all_on, FLIP, 4 },
	{ s_clock, NULL, &effect_clock, FLIP, 4 },
	{ s_tornado, NULL, &effect_tornado, FLIP, 4 },
	{ s_lines, NULL, &effect_lines, NO_FLIP, 4 },
	{ s_tube, NULL, &effect_tube, FLIP, 4 },
	{ s_rain, &init_rain, &effect_rain, FLIP, 4 },
	{ s_stairs_x, &init_stairs_x, NULL, NO_FLIP, 4 },
	{ s_character, NULL, &effect_character, NO_FLIP, 4 },
	{ s_wave, NULL, &effect_wave, FLIP, 4 },
	{ s_circle, NULL, &effect_circle, FLIP, 4 },
	{ s_template, &init_template, &effect_template, FLIP, 4 },
	{ s_layers, NULL, &effect_layers, FLIP, 4 },
	{ s_constant, NULL, &effect_constant, FLIP, 4 },
	{ s_sphere, NULL, &effect_sphere, FLIP, 4 },
	{ s_game_of_life, &init_game_of_life, &effect_game_of_life, FLIP, 12 },
	{ s_worm, &init_worm, &effect_worm, NO_FLIP, 4 },
	{ s_cube, &init_cube, NULL, NO_FLIP, 4 },
	{ s_brownian, &init_brownian, &effect_brownian, NO_FLIP, 4 },
	{ s_wireframe, NULL, &effect_wireframe, NO_FLIP, 4 },
	{ s_countdown, &init_countdown, &effect_countdown, FLIP, 125 },
	{ s_line, &init_line, NULL, NO_FLIP, 4 },
	{ s_starfield, &init_starfield, &effect_starfield, NO_FLIP, 4 },
	{ s_fish, NULL, &effect_fish, FLIP, 4 },
	{ s_test_sensor1, NULL, &effect_test_sensor1, FLIP, 4 },
	{ s_particles, &init_particles, &effect_particles, NO_FLIP, 4 },
	{ s_scroll_text, NULL, &effect_scroll_text, FLIP, 4 },
};

const uint8_t effects_len = sizeof(effects) / sizeof(effect_t);

void effect_gradient(void){
	float fac;
	int8_t cur = (ticks >> 5) % 8;
	clear_buffer();
	for(int8_t i = 0; i < LEDS_X; i++) {
		for(uint8_t j = 0; j < LEDS_Z; j++) {
			fac = j <= cur? (cur - j) * 0.1: 0.0;
			set_row(i, LEDS_Z - 1 - j, 0, 7, MAX_INTENSITY * fac);
		}
	}
}

XY(effect_sine) {
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;
	uint16_t i = scaler * (2 + sin((float)x / 2 + (float)ticks / 25) + sin((float)y / 2 + (float)ticks / 50));
	set_z(x, y, i);
}

static void init_stairs_y(void){
	assert(LEDS_Y <= LEDS_Z);
	
	clear_buffer();
	for(uint8_t x = 0; x<LEDS_X; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led_8_8_12(x, y, y, MAX_INTENSITY);
		}
	}
}

static const uint8_t matrix_xyz_len = 10;
static void init_matrix(void){
	for(uint8_t i = 0; i < matrix_xyz_len; i++) {
		vars.matrix.xyz[i] = (xyz_t){
			.x = randint(0, LEDS_X),
			.y = randint(0, LEDS_Y),
			.z = randint(0, LEDS_Z)
		};
	}
	clear_buffer();
}
void effect_matrix(void){
	clear_buffer();
	for(uint8_t i = 0; i < matrix_xyz_len; i++) {
		xyz_t xyz = vars.matrix.xyz[i];
		for(uint8_t j = 0; j < 3; j++) {
			if(xyz.z + j < LEDS_Z) {
				set_led(xyz.x, xyz.y, xyz.z + j, MAX_INTENSITY);
			}
		}
		uint8_t z = xyz.z;
		z++;
		if(z >= LEDS_Z) z = 0;
		vars.matrix.xyz[i].z = z;
	}
}

static void init_heart(void){
	vars.heart.y = 255;
}
void effect_heart(void){
	clear_buffer();
	
	vars.heart.y -= (160-sensors.distance1+40)/10;
	heart_shape(vars.heart.y);
}

static void init_finite_worm(void){
	vars.finite_worm.pos[0] = 4;
	vars.finite_worm.pos[1] = 4;
	vars.finite_worm.pos[2] = 4;
	vars.finite_worm.dir = 0;
	vars.finite_worm.speed = 1;
	memset(vars.finite_worm.prev_dirs, 10, WORM_LENGTH);
	vars.finite_worm.prev_dir_i = 0;
	clear_buffer();
}
void effect_finite_worm(void){
	clear_buffer();
	set_led(vars.finite_worm.pos[0], vars.finite_worm.pos[1], vars.finite_worm.pos[2], MAX_INTENSITY);
	
	if(vars.finite_worm.prev_dirs[0] != 10) {
		int8_t i;
		uint8_t j;
		uint16_t tmp_pos[3] = {vars.finite_worm.pos[0], vars.finite_worm.pos[1], vars.finite_worm.pos[2]};
		for(i = vars.finite_worm.prev_dir_i - 1, j = 0; j < WORM_LENGTH; i--, j++) {
			if(i == -1) i = WORM_LENGTH-1;
			tmp_pos[vars.finite_worm.prev_dirs[i]] -= vars.finite_worm.prev_speeds[i];
			set_led(tmp_pos[0], tmp_pos[1], tmp_pos[2], MAX_INTENSITY);
		}
	}
	
	
	int new_pos = vars.finite_worm.pos[vars.finite_worm.dir] + vars.finite_worm.speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		vars.finite_worm.dir = vars.finite_worm.dir + 1 > 2? 0: vars.finite_worm.dir + 1;
		vars.finite_worm.speed = -vars.finite_worm.speed;
	}
	vars.finite_worm.prev_dirs[vars.finite_worm.prev_dir_i] = vars.finite_worm.dir;
	vars.finite_worm.prev_speeds[vars.finite_worm.prev_dir_i] = vars.finite_worm.speed;
	if(vars.finite_worm.prev_dir_i < WORM_LENGTH-1) {
		vars.finite_worm.prev_dir_i++;
	}
	else {
		vars.finite_worm.prev_dir_i = 0;
	}
	vars.finite_worm.pos[vars.finite_worm.dir] += vars.finite_worm.speed;
}

XYZ(effect_all_on) {
	set_led(x, y, z, MAX_INTENSITY);
}

void effect_clock(void){
	char text[9];
	text[0] = 8;
	text[3] = ':';
	text[6] = ':';
	time_t now = time(NULL);
	time_t since_midnight = (now+TIMEZONE_SECS) % SECS_IN_DAY;
	uint8_t secs = since_midnight%60;
	uint8_t minutes = since_midnight/60%60;
	uint8_t hours = since_midnight/60/60;
	text[1] = '0'+(hours/10);
	text[2] = '0'+(hours%10);
	text[4] = '0'+(minutes/10);
	text[5] = '0'+(minutes%10);
	text[7] = '0'+(secs/10);
	text[8] = '0'+(secs%10);
	clear_buffer();
	int16_t pos = ticks >> 3;
	scroll_text(text, false, pos, render_xy);
	scroll_text(text, false, pos-7, render_yz);
}

void effect_tornado(void) {
	int8_t fac = (ticks >> 4) % 2;
	clear_buffer();
	circle_shape(fac, 0, 0, 9, 13, MAX_INTENSITY);
	circle_shape(-fac, 0, 1, 8, 12, MAX_INTENSITY);
	circle_shape(0, fac, 2, 7, 11, MAX_INTENSITY);
	circle_shape(0, -fac, 3, 7, 10, MAX_INTENSITY);
	circle_shape(fac, 0, 4, 6, 9, MAX_INTENSITY);
	circle_shape(-fac, 0, 5, 3, 5, MAX_INTENSITY);
	circle_shape(0, fac, 6, 1, 3, MAX_INTENSITY);
	set_led(fac, 4, 7, MAX_INTENSITY);
}

void effect_lines(void) {
	if(!(ticks % 10))
		line(randint(0, 7), randint(0, 7), randint(0, 7),
			randint(0, 7), randint(0, 7), randint(0, 7),
			MAX_INTENSITY);
}

void effect_tube(void) {
	clear_buffer();
	circle_shape(0, 0, (ticks >> 2) % LEDS_Z, 6, 10, MAX_INTENSITY);
}

static void init_rain(void){
	vars.rain.seed = rand();
}
void effect_rain(void){
	
	
	clear_buffer();
	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			srand(3*(x*LEDS_Y+y));
			set_led(x,y,((ticks >> 3)+rand()) & 7, MAX_INTENSITY >> 3);
		}
	}
	const uint8_t water = ((ticks >> 7) % 7) + 1;
	const uint8_t surface = ticks & 127;
	for (uint8_t z=0; z<water; z++) {
		for (uint8_t x=0; x<8; x++) {
			for (uint8_t y=0; y<8; y++) {
				set_led(x,y,7-z, MAX_INTENSITY);
			}
		}
	}
	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			set_led(x,y,8-water, weber_fechner(surface << 1));
		}
	}
}

static void init_stairs_x(void){
	clear_buffer();
	for(uint8_t i = 0; i < 8; i++) {
		set_row(i, i, 0, 7, MAX_INTENSITY);
	}
}

void effect_character(void){
	render_character('c', 0, render_yz);
}

XY(effect_wave) {
	vars.wave.avg = (0.9*vars.wave.avg)+(0.1*(float)sensors.distance1);
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;
	uint16_t i = scaler * (2 + sin((float)x * 50 + (float)ticks / 15 + vars.wave.avg/10));
	set_z(x, y, i);
}

void effect_circle(void) {
	circle_shape(0, 0, 0, 6, 10, MAX_INTENSITY);
}

static void init_template(void){
}
void effect_template(void){
}

void effect_layers(void){
	clear_buffer();
	uint8_t z = ((ticks >> 7) % LEDS_Z);
	for(uint8_t x=0; x<LEDS_X; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
}

XY(effect_constant) {
	set_led(x, y, 3, MAX_INTENSITY);
}

void effect_sphere(void){
	float fac = (float)((ticks >> 3) % 10) / 10;
	clear_buffer();
	sphere_shape(-3, -3, -3, 10, 14, fac);
}

void set_leds(uint8_t x, uint8_t y, uint8_t z)
{
	uint8_t neighbours = get_amount_of_neighbours((int8_t)x, (int8_t)y, (int8_t)z);
	if((neighbours >= 6 && neighbours <= 15) || randint(0, 10) > 8) {
		set_led(x, y, z, MAX_INTENSITY);
		vars.game_of_life.is_alive = 1;
	}
	else set_led(x, y, z, 0);
}
static uint8_t get_amount_of_neighbours(uint8_t x, uint8_t y, uint8_t z) {
	uint8_t ret = 0;
	for(int8_t cx = -1; cx <= 1; cx++)
		for(int8_t cy = -1; cy <= 1; cy++)
			for(int8_t cz = -1; cz <= 1; cz++)
				ret += get_led_wrap(x + cx, y + cy, z + cz) > 0;
	return ret;
}
static void init_game_of_life(void){
	
	heart_shape(WEBER_FECHNER_MAX);
}
void effect_game_of_life(void) {
	vars.game_of_life.is_alive = 0;
	iterate_xyz(set_leds);
	if(!vars.game_of_life.is_alive) {
		heart_shape(WEBER_FECHNER_MAX);
	}
}

static void init_worm(void){
	vars.worm.pos[0] = 4;
	vars.worm.pos[1] = 4;
	vars.worm.pos[2] = 4;
	vars.worm.dir = 0;
	vars.worm.speed = 1;
	clear_buffer();
}
void effect_worm(void){
	set_led(vars.worm.pos[0], vars.worm.pos[1], vars.worm.pos[2], MAX_INTENSITY);
	
	
	int new_pos = vars.worm.pos[vars.worm.dir] + vars.worm.speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		vars.worm.dir = vars.worm.dir + 1 > 2? 0: vars.worm.dir + 1;
		vars.worm.speed = -vars.worm.speed;
	}
	vars.worm.pos[vars.worm.dir] += vars.worm.speed;
}

static void init_cube(void) {
	clear_buffer();
	cube_shape(0, 0, 0, 7, 7, 7, MAX_INTENSITY);
}

static void init_brownian(void){
	vars.brownian.xyz = (xyz_t){
		.x = LEDS_X / 2,
		.y = LEDS_Y / 2,
		.z = LEDS_Z / 2
	};
	clear_buffer();
}
void effect_brownian(void){
	set_led(vars.brownian.xyz.x, vars.brownian.xyz.y, vars.brownian.xyz.z, MAX_INTENSITY);
	vars.brownian.xyz = (xyz_t){
		.x = clamp(vars.brownian.xyz.x + randint(-2, 2), 0, LEDS_X - 1),
		.y = clamp(vars.brownian.xyz.y + randint(-2, 2), 0, LEDS_Y - 1),
		.z = clamp(vars.brownian.xyz.z + randint(-2, 2), 0, LEDS_Z - 1)
	};
}

void effect_wireframe(void) {
	uint8_t a = (ticks >> 5) & 7;
	uint8_t b = 7 - ((ticks >> 5) & 7);
	clear_buffer();
	cube_shape(a, a, a, b, b, b, MAX_INTENSITY);
}

static void init_countdown(void) {
	vars.countdown.cur = 15;
}
void effect_countdown(void){
	char text[3];
	text[0] = 2;
	text[1] = '0' + (vars.countdown.cur / 10);
	text[2] = '0' + (vars.countdown.cur % 10);
	clear_buffer();
	scroll_text(text, false, 9, render_yz);
	scroll_text(text, false, 16, render_xy);
	if(vars.countdown.cur > 0) vars.countdown.cur--;
}

static void init_line(void) {
	clear_buffer();
	line(1, 2, 0, 4, 7, 6, MAX_INTENSITY);
}

static const uint8_t starfield_xyz_len = 10;
static void init_starfield(void){
	for(uint8_t i = 0; i < starfield_xyz_len; i++) {
		vars.starfield.xyz[i] = (xyz_t){
			.x = randint(0, LEDS_X),
			.y = randint(0, LEDS_Y),
			.z = randint(0, LEDS_Z)
		};
	}
	clear_buffer();
}
void effect_starfield(void){
	clear_buffer();
	for(uint8_t i = 0; i < starfield_xyz_len; i++) {
		xyz_t xyz = vars.starfield.xyz[i];
		set_led(xyz.x, xyz.y, xyz.z, MAX_INTENSITY);
		uint8_t y = xyz.y;
		y++;
		if(y >= LEDS_Y) y = 0;
		vars.starfield.xyz[i].y = y;
	}
}

void effect_fish(void) {
	fish_shape(2, 1, 1, MAX_INTENSITY);
}

void effect_test_sensor1(void){
	clear_buffer();
	uint8_t z = sensors.distance1 / 20;
	if (z >= LEDS_Z)
		z = LEDS_Z - 1;
	for(uint8_t x=0; x<2; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
	z = sensors.distance2 / 20;
	if (z >= LEDS_Z)
		z = LEDS_Z - 1;
	for(uint8_t x=2; x<4; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
	z = sensors.ambient_light / 32;
	for(uint8_t x=4; x<6; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
	
	z = sensors.sound_pressure_level / 32;
	for(uint8_t x=6; x<LEDS_X; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
}

static const uint8_t xyz_len = 5;
static void init_particles(void){
	for(uint8_t i = 0; i < xyz_len; i++) {
		xyz_t p = {
			.x=randint(0, LEDS_X),
			.y=randint(0, LEDS_Y),
			.z=randint(0, LEDS_Z)
		};
		vars.particles.xyz[i] = p;
	}
	clear_buffer();
}
void effect_particles(void){
	if(ticks % 50) return;
	clear_buffer();
	for(uint8_t i = 0; i < xyz_len; i++) {
		xyz_t p = vars.particles.xyz[i];
		set_led(p.x, p.y, p.z, MAX_INTENSITY);
		p.x = clamp(p.x + randint(-1, 1), 0, LEDS_X - 1);
		p.y = clamp(p.y + randint(-1, 1), 0, LEDS_Y - 1);
		p.z = clamp(p.z + randint(-1, 1), 0, LEDS_Z - 1);
		vars.particles.xyz[i] = p;
	}
}

PROGMEM static const char default_text[] = "\x05""ERROR";
void effect_scroll_text(void){
	
	const char *text = custom_data == NULL? default_text: (const char*)custom_data;
	clear_buffer();
	int16_t pos = ticks >> 3;
	scroll_text(text, true, pos, render_xy);
	scroll_text(text, true, pos-7, render_yz);
}

