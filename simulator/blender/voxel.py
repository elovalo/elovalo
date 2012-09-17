#
# Copyright 2012 Elovalo project group 
# 
# This file is part of Elovalo.
# 
# Elovalo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Elovalo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
#

# Voxel helpers. Using these you can convert real animation into a voxel
# one using the led cube as the domain.
import bpy
from utils import led_obs, ob, clear, turn_on


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


def update(ctx):
    clear()
    c = ob('Cube')
    voxelize(c)  # XXX: expensive, try caching somehow

# exec on frame change
while len(bpy.app.handlers.frame_change_pre):
    bpy.app.handlers.frame_change_pre.pop()

bpy.app.handlers.frame_change_pre.append(update)
