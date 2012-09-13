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

import bpy


def get_selected():
    return bpy.context.selected_editable_objects


def cube():
    bpy.obs.mesh.primitive_cube_add()
    return get_selected()[0]


def material(n, o=None):
    if o:
        o.active_material = material(n)
        return

    return bpy.data.materials[n]


def duplicate(n):
    mat = material(n)

    bpy.ops.material.new()
    ret = bpy.data.materials[-1]

    for n in filter(lambda a: not a.startswith('__'), dir(mat)):
        try:
            setattr(ret, n, getattr(mat, n))
        except AttributeError:
            pass  # skip read-only cases

    return ret


def ob(n):
    scn = bpy.data.scenes[0]
    return [a for a in scn.objects if a.name == n][0]


def chunked(g, amount):
    ret = []

    for i, n in enumerate(g):
        ret.append(n)

        if not ((i + 1) % amount):
            yield ret
            ret = []
