# IMPORTANT!!!! Execute this file first (alt-p)
# before animating

from functools import partial
import json
import os
from leds import led_obs

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

hd = os.getenv('hd')

if hd:
    render.resolution_percentage = 100

fps = os.getenv('fps')

if fps:
    render.fps = int(fps)

length = os.getenv('length')

if length:
    bpy.context.scene.frame_end = int(length) * render.fps

# exec on frame change
while len(bpy.app.handlers.frame_change_pre):
    bpy.app.handlers.frame_change_pre.pop()


def load_data():
    p = os.path.join(CUR_PATH, FX_DIR, JSON + '.json')

    with open(p) as f:
        d = json.load(f)

    return d


def update(frames, scene):
    def render_frame(i):
        i = i % len(frames) - 1

        states = frames[i]

        for led_ob, alpha in zip(led_obs(), states):
            led_ob.active_material.alpha = alpha

    render_frame(scene.frame_current)

bpy.app.handlers.frame_change_pre.append(
        partial(update, load_data()['frames']))