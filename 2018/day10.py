""" Idea: find the message with the smallest height!
"""
import re

with open('input/day10.txt') as fh:
    lines = fh.readlines()

ps = []
vs = []
for line in lines:
    regex = 'position=<(.*)> velocity=<(.*)>'
    r = re.search(regex, line)
    x, y = [int(x.strip()) for x in r.group(1).strip().split(',')]
    ps.append((x, y))
    x, y = [int(x.strip()) for x in r.group(2).strip().split(',')]
    vs.append((x, y))

def render(ps):
    minx = min([x[0] for x in ps])
    maxx = max([x[0] for x in ps])
    miny = min([x[1] for x in ps])
    maxy = max([x[1] for x in ps])

    g = []

    for yidx, y in enumerate(range(maxy - miny + 1)):
        g.append([])
        for x in range(maxx - minx + 1):
            g[yidx].append('.')

    for x, y in ps:
        g[y - miny][x - minx] = '#'

    # return g
    for r in g:
        for c in r:
            print(c, end='')
        print()

def size(ps):
    minx = min([x[0] for x in ps])
    maxx = max([x[0] for x in ps])
    miny = min([x[1] for x in ps])
    maxy = max([x[1] for x in ps])

    return (maxx - minx + 1, maxy - miny + 1)

def shift(ps, vs):
    return [(p[0] + v[0], p[1] + v[1]) for p, v in zip(ps, vs)]

my = 100e100
_ps = None
mn = 0
for n in range(20000):
    _, y = size(ps)
    if y < my:
        my = y
        mn = n
        _ps = ps
    ps = shift(ps, vs)

render(_ps)
print(mn)
