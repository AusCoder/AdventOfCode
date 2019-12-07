from collections import deque


class Creature:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.hp = 200

    def find_target(self, enemies):
        min_hp = 10e10
        target = None
        for nx, ny in neighbours(self.x, self.y):
            if (nx, ny) in enemies:
                creature = enemies[(nx, ny)]
                # ordering of neighbours takes care of reading order (I think)
                if creature.hp < min_hp:
                    target = creature
                    min_hp = creature.hp
        return target

    def path_to_nearest(self, dungeon, alies, enemies):
        closest_enemies = []
        min_dist = 10e10
        found_min_dist = False
        found_greater_than_min_dist = False
        dists = {}
        q = deque()
        dists[(self.x, self.y)] = 0
        q.append((self.x, self.y, []))
        while not found_greater_than_min_dist and q:
            x, y, path = q.popleft()
            for nx, ny in neighbours(x, y):
                if (nx, ny) in dists:
                    continue
                if dungeon[ny][nx] != '.':
                    continue
                if (nx, ny) in alies:
                    continue
                if (nx, ny) in enemies:
                    # if found_min_dist is False:
                    #  set to true, set min_dist
                    # if dist[(x, y)] == min_dist
                    #  append
                    # if dist[(x, y)] > min_dist:
                    #  found_greater_than_min_dist = True break
                    if not found_min_dist:
                        found_min_dist = True
                        min_dist = dists[(x, y)]
                    if found_min_dist and dists[(x, y)] == min_dist:
                        closest_enemies.append(((nx, ny), path))
                    if found_min_dist and dists[(x, y)] > min_dist:
                        found_greater_than_min_dist = True
                        break
                dists[(nx, ny)] = dists[(x, y)] + 1
                npath = path + [(nx, ny)]
                q.append((nx, ny, npath))

        closest_enemies.sort(key=lambda t: (t[0][1], t[0][0]))
        if closest_enemies:
            return closest_enemies[0][1]
        return None

    def _old_path_to_nearest(self, dungeon, alies, enemies):
        found = None
        dists = {}
        q = deque()
        dists[(self.x, self.y)] = 0
        q.append((self.x, self.y, []))
        while found is None and q:
            x, y, path = q.popleft()
            for nx, ny in neighbours(x, y):
                if (nx, ny) in dists:
                    continue
                if (nx, ny) in enemies:
                    found = path
                    break
                if dungeon[ny][nx] != '.':
                    continue
                if (nx, ny) in alies:
                    continue
                dists[(nx, ny)] = dists[(x, y)] + 1
                npath = path + [(nx, ny)]
                q.append((nx, ny, npath))
        return found

def neighbours(x, y):
    return [
        (x, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x, y + 1),
    ]

def read_input():
    with open('input/day15.txt') as fh:
        lines = fh.readlines()

    elfs = {}
    gobs = {}
    dungeon = []

    for i1, line in enumerate(lines):
        dungeon.append([])
        for i2, c in enumerate(line):
            if c in 'EG':
                creature = Creature(i2, i1)
                if c == 'E':
                    elfs[(i2, i1)] = creature
                else:
                    gobs[(i2, i1)] = creature
                dungeon[i1].append('.')
            if c in '#.':
                dungeon[i1].append(c)

    return elfs, gobs, dungeon

def print_creatures(name, creatures):
    for p in creatures:
        creat = creatures[p]
        assert creat.x == p[0]
        assert creat.y == p[1]
        print('{}: ({}, {}). hp: {}'.format(name, creat.x, creat.y, creat.hp))

def run_creatures(dungeon, elfs, gobs):
    # for each creature:
        #   if can attack:
        #     attack
        #   if can move:
        #     move
        #   else:
        #     do nothing
    positions = list(elfs.keys()) + list(gobs.keys())
    positions.sort(key=lambda p: (p[1], p[0]))

    for pos in positions:
        if pos in elfs:
            alies = elfs
            enemies = gobs
        else:
            alies = gobs
            enemies = elfs
        if pos not in alies:  # this creature was killed, skip this position
            continue

        is_combat_ended = run_creature(dungeon, pos, alies, enemies)
        if is_combat_ended:
            return True
    return False

def run_creature(dungeon, pos, alies, enemies):
    if not enemies:
        return True
    goodie = alies[pos]
    path = goodie.path_to_nearest(dungeon, alies, enemies)
    if path:
        alies.pop(pos)
        new_pos = path[0]
        goodie.x = new_pos[0]
        goodie.y = new_pos[1]
        alies[new_pos] = goodie

    target = goodie.find_target(enemies)
    if target:
        target.hp -= 3
        if target.hp <= 0:
            enemies.pop((target.x, target.y))
    return False


def part1():
    elfs, gobs, dungeon = read_input()

    turn_num = 0
    # for _ in range(2):
    #     run_creatures(dungeon, elfs, gobs)
    while not run_creatures(dungeon, elfs, gobs):
        turn_num += 1
    assert not elfs or not gobs

    total_hp = 0
    for _, e in elfs.items():
        total_hp += e.hp
    for _, g in gobs.items():
        total_hp += g.hp

    print(turn_num)
    print(total_hp * turn_num)

    for i1, row in enumerate(dungeon):
        for i2, col in enumerate(row):
            if (i2, i1) in elfs:
                print('E', end='')
            elif (i2, i1) in gobs:
                print('G', end='')
            else:
                print(col, end='')
        print()

    print_creatures('Elf', elfs)
    print_creatures('Gob', gobs)

part1()

# tried these:
# 228272
# 88 - 227744 - too low
# try this:
