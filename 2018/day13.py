from collections import deque
import numpy as np

class Car:
    def __init__(self, pos: np.ndarray, d: np.ndarray):
        self.pos = pos
        self.dir = d
        self.turns = 1
        self.is_removed = False

# \ is this transformation
# [1, 0] -> [0, 1]
# [0, 1] -> [1, 0]
trans = {}
trans['\\'] = np.array([[0, 1], [1, 0]], dtype=int)
# / is this transformation
# [1, 0] -> [0, -1]
# [0, -1] -> [1, 0]
trans['/'] = np.array([[0, -1], [-1, 0]], dtype=int)

trans['l'] = np.array([[0, 1], [-1, 0]], dtype=int)
trans['r'] = np.array([[0, -1], [1, 0]], dtype=int)
trans['s'] = np.identity(2, dtype=int)

directions = {
    'v': np.array([0, 1], dtype=int),
    '^': np.array([0, -1], dtype=int),
    '<': np.array([-1, 0], dtype=int),
    '>': np.array([1, 0], dtype=int),
}

with open('input/day13.txt') as fh:
    lines = fh.readlines()

tracks = {}
cars = []
for y in range(len(lines)):
    for x in range(len(lines[y])):
        c = lines[y][x]
        if c in '\\/+':
            tracks[(x, y)] = c
        if c in 'v^<>':
            cars.append(Car(
                np.array([x, y]),
                directions[c],
            ))

def turn_car(car, tracks):
    p = tuple(car.pos)
    if p in tracks:
        if tracks[tuple(car.pos)] == '+':
            if car.turns % 3 == 0:
                car.dir = np.dot(trans['r'], car.dir)
            if car.turns % 3 == 1:
                car.dir = np.dot(trans['l'], car.dir)
            if car.turns % 3 == 2:
                car.dir = np.dot(trans['s'], car.dir)
            car.turns += 1
        else:
            car.dir = np.dot(trans[tracks[p]], car.dir)

def print_track(cars, tracks):
    for y in range(len(tracks)):
        for x in range(len(tracks[y])):
            f = False
            for c in cars:
                if (x, y) == c[:2]:
                    if tuple(c[-1]) == (0, 1):
                        print('v', end='')
                    if tuple(c[-1]) == (0, -1):
                        print('^', end='')
                    if tuple(c[-1]) == (1, 0):
                        print('>', end='')
                    if tuple(c[-1]) == (-1, 0):
                        print('<', end='')
                    f = True
                    break
            if not f:
                print(tracks[y][x], end='')
        print()

def part1():
    tick = 1
    while True:
        # print_track(cars, tracks)
        # print()

        cars.sort(key=lambda c: (c.pos[1], c.pos[0]))
        for i1, c1 in enumerate(cars):
            c1.pos += c1.dir

            founds = [c2 for i2, c2 in enumerate(cars) if i1 != i2 and np.all(c1.pos == c2.pos)]
            if founds:
                print('tick: {}. {},{}'.format(tick, c1.pos[0], c1.pos[1]))
                break
            turn_car(c1, tracks)

        if founds:
            break
        tick += 1

def part2():
    tick = 1
    num_alive = len([c for c in cars if not c.is_removed])
    while num_alive > 1:
        # print_track(cars, tracks)
        # print()

        cars.sort(key=lambda c: (c.pos[1], c.pos[0]))
        for i1, c1 in enumerate(cars):
            c1.pos += c1.dir

            founds = [c2 for i2, c2 in enumerate(cars) if i1 != i2 and np.all(c1.pos == c2.pos)]

            for c2 in founds:
                if c1.is_removed or c2.is_removed:
                    continue
                c1.is_removed = True
                c2.is_removed = True

            turn_car(c1, tracks)

        num_alive = len([c for c in cars if not c.is_removed])
        tick += 1

    lastcar = [c for c in cars if not c.is_removed][0]
    print('tick: {}. last car pos: {},{}'.format(tick, lastcar.pos[0], lastcar.pos[1]))


part2()
