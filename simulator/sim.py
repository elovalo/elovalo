# IMPORTANT!!!! Execute this file first (alt-p)
# before animating

from functools import partial
import itertools
import json
import os

import bpy

FX_DIR = 'exports'
JSON = os.getenv('effect') or 'brownian'

CUR_PATH = os.path.split(bpy.data.filepath)[0]

render = bpy.data.scenes[0].render
render_path = os.getenv('path')

if render_path:
    render_path = render_path if render_path[-1] == '/' else render_path + '/'
    render.filepath = os.path.join(CUR_PATH, render_path)

def stamp_note(cur, fx):
    return cur.split('-')[0].strip() + ' - ' + fx.title()

render.stamp_note_text = stamp_note(render.stamp_note_text, JSON)

# TODO: might want to link mesh data (link mat to ob)

# http://blenderpythonscripts.wordpress.com/2011/04/24/cursor-control-0-6-0/
def space():
    area = None
    for area in bpy.data.window_managers[0].windows[0].screen.areas:
        if area.type == 'VIEW_3D':
            break
    if area.type != 'VIEW_3D':
        return None
    for space in area.spaces:
        if space.type == 'VIEW_3D':
            break
    if space.type != 'VIEW_3D':
        return None
    return space

def set_cursor(*coordinates):
    space().cursor_location = coordinates

def get_cursor():
    return space().cursor_location

def get_selected():
    return bpy.context.selected_editable_objects

def cube():
    bpy.ops.mesh.primitive_cube_add()
    return get_selected()[0]

def material(n, o=None):
    if o:
        o.active_material = material(n)
        return

    return bpy.data.materials[n]

def toggle(o):
    if o.active_material is None or o.active_material.name == 'led_on':
        material('led_off', o)
    else:
        material('led_on', o)

def ob(n):
    scn = bpy.data.scenes[0]
    return [a for a in scn.objects if a.name == n][0]

def toggle_led(i):
    toggle(ob('led_' + str(i)))

def toggle_z(i):
    offset = i * 8
    [toggle_led(j) for j in range(offset, offset + 8)]

def leds():
    prefix = 'led'
    n = 8
    for i in range(n * n * n):
        yield prefix + '_' + str(i)

def led_obs():
    for l in leds():
        yield ob(l)

def duplicate(n):
    mat = material(n)
    
    bpy.ops.material.new()
    ret = bpy.data.materials[-1]

    for n in filter(lambda a: not a.startswith('__'), dir(mat)):
        try:
            setattr(ret, n, getattr(mat, n))
        except AttributeError:
            pass # skip read-only cases

    return ret

# TODO: make this use leds() generator
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

def turn_off(o):
    material('led_off', o)

def turn_on(o):
    material('led_on', o)

def clear():
    for led_ob in led_obs():
        turn_off(led_ob)

def sdist(a, b):
    return a * a + b * b

def voxelize(o, is_set=None):
    is_set = is_set or turn_on
    delta = 0.2
    imat = o.matrix_world.inverted()

    for led_ob in led_obs():
        loc = imat * led_ob.location
        for e in o.data.edges.values():
            verts = e.vertices
            v1 = imat * o.data.vertices[verts[0]].co
            v2 = imat * o.data.vertices[verts[1]].co

            if sdist(v1, loc) + sdist(v2, loc) < sdist(v1, v2) + delta:
                is_set(led_ob)
                break

def chunked(g, amount):
    ret = []
    
    for i, n in enumerate(g):
        ret.append(n)

        if not ((i + 1) % amount):
            yield ret
            ret = []

def export_bytes(o):
    clear()
    
    scn = bpy.data.scenes[0]

    with open('/tmp/export.bin', 'wt', encoding='utf8') as f:
        for i in range(scn.frame_start, scn.frame_end):
            print('writing %s' % i)
            scn.frame_current = i
            voxelize(o)

            for chunk in chunked(led_obs(), 8):
                byte = int(''.join([str(int(c.active_material.name == 'led_on')) for c in chunk]), 2)
                f.write('%c' % byte)

    print('export done!')

# run only once :)
#generate_leds()

# toggle tests
#toggle_led(13)
#toggle_z(3)

def update(states, ctx):
    #scn = bpy.data.scenes[0]

    #state = states[scn.frame_current - scn.frame_start]
    
    clear()
    c = ob('Cube')
    voxelize(c) # cache here instead???
    #[turn_on(o) for o in state]

def generate_states(o):
    generate_states.ret = []
    scn = bpy.data.scenes[0]

    def is_set(led_ob):
        generate_states.ret[-1].append(led_ob)

    for i in range(scn.frame_start, scn.frame_end):
        scn.frame_current = i
        # TODO: figure out how refresh properly
        
        print('Caching frame', scn.frame_current)

        generate_states.ret.append([])        
        voxelize(o, is_set)

    generate_states.ret.append([])

    return generate_states.ret

# exec on frame change
while len(bpy.app.handlers.frame_change_pre):
    bpy.app.handlers.frame_change_pre.pop()

#c = ob('Cube')
#clear()
#voxelize(c)

#export_bytes(c)

#states = [] #generate_states(c)
#bpy.app.handlers.frame_change_pre.append(partial(update, states))

def load_data():
    p = os.path.join(CUR_PATH, FX_DIR, JSON + '.json')

    with open(p) as f:
        d = json.load(f)

    # fps = data['fps'] TODO: set. 25 by default for now
    # TODO: deal with geometry too

    return d

def update2(frames, scene):
    def render_frame(i):
        i = i % len(frames) - 1
    
        states = frames[i]
        
        for led_ob, alpha in zip(led_obs(), states):
            led_ob.active_material.alpha = alpha

    render_frame(scene.frame_current)    

# use this to generate initial leds
#generate_leds('led_on')

bpy.app.handlers.frame_change_pre.append(partial(update2, load_data()['frames']))

#print('done')
