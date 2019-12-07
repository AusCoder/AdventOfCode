"""
    (((x + 10) * y + serial) * (x + 10)) // 100 % 10 - 5
"""
import numpy as np


serial = 9445

cs = np.zeros((300, 300), dtype=int)

for _y in range(300):
    for _x in range(300):
        y = _y + 1
        x = _x + 1
        cs[_y][_x] = (((x + 10) * y + serial) * (x + 10)) // 100 % 10 - 5

# print(cs[47 - 1, 33 - 1])

m = -100e100
coords = None
for ty in range(298):
    for tx in range(298):
        s = np.sum(cs[ty:ty+3, tx:tx+3])

        if s > m:
            m = s
            coords = (tx + 1, ty + 1)

# print(m)
# print(coords)

# dumb
m = -100e100
coords = None
for n in range(1, 301):
    for ty in range(300 - n):
        for tx in range(300 - n):
            s = np.sum(cs[ty:ty+n, tx:tx+n])

            if s > m:
                m = s
                coords = (tx + 1, ty + 1, n)

print(m)
print(coords)
