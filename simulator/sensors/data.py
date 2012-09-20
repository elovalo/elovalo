def constant(i):
    return lambda a: i

sensors = {
    "distance1": constant(1),
    "distance2": constant(1),
    "ambient_light": constant(1),
    "sound_pressure": constant(1),
}
