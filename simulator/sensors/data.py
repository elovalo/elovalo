def constant(i):
    return lambda a: i

def rising(i):
    return lambda a: a * i % 1023

sensors = {
    "distance1": rising(1),
    "distance2": constant(512),
    "ambient_light": constant(1),
    "sound_pressure": constant(1),
}
