import itertools
from utils import ob, material, cube, duplicate


# TODO: make this use leds() generator
# TODO: might want to link obs to same mesh data
def generate_leds(mat='led_off'):
    prefix = 'led'
    n = 8
    s = 0.01
    r = [i / (n - 1) for i in range(n)]

    for i, axes in enumerate(itertools.product(r, r, r)):
        o = cube()
        o.name = prefix + '_' + str(i)
        o.location = axes
        o.scale = (s, s, s)
        o.show_transparent = True

        o.active_material = duplicate(mat)


def turn_on(o):
    material('led_on', o)


def turn_off(o):
    material('led_off', o)


def leds():
    prefix = 'led'
    n = 8
    for i in range(n * n * n):
        yield prefix + '_' + str(i)


def led_obs():
    for l in leds():
        yield ob(l)


def clear():
    [turn_off(led_ob) for led_ob in led_obs()]
