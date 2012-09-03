import json
import random

j = {
    "fps": 25,
    "geometry": [8,8,8],
    "frames": []
}

d = []
for i in range(0, 100):
    d.append(random.random())

j["frames"].append(d)

f = open('test.json', 'a')
f.write(json.dumps(j))
f.close()
