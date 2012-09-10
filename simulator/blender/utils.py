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
