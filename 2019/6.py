from collections import defaultdict
from pathlib import Path

ROOT = "COM"


def count_orbits(orbit_map):
    centers = defaultdict(list)

    for relationship in orbit_map:
        center, orbitee = relationship.split(")")
        centers[center].append(orbitee)

    direct, indirect = _count(centers, -1, ROOT)
    print(direct + indirect + 1)


def _count(centers, indirect_value, cur_object):
    direct = len(centers[cur_object])
    indirect = indirect_value

    for obj in centers[cur_object]:
        d, i = _count(centers, indirect_value + 1, obj)
        direct += d
        indirect += i

    return direct, indirect


def part1():
    orbit_map = Path("input/6.txt").read_text().strip().split("\n")
    count_orbits(orbit_map)


def num_orbit_transfers(orbit_map, you, san):
    orbitees = dict()

    for relationship in orbit_map:
        center, orbitee = relationship.split(")")
        orbitees[orbitee] = center

    parents_of_you = dict()
    cur_node = orbitees[you]
    cur_steps = 0
    while True:
        parents_of_you[cur_node] = cur_steps
        if cur_node == ROOT:
            break
        cur_node = orbitees[cur_node]
        cur_steps += 1

    cur_node = orbitees[san]
    cur_steps = 0
    while cur_node not in parents_of_you:
        cur_node = orbitees[cur_node]
        cur_steps += 1

    print(cur_steps + parents_of_you[cur_node])


def part2():
    orbit_map = Path("input/6.txt").read_text().strip().split("\n")
    num_orbit_transfers(orbit_map, "YOU", "SAN")


if __name__ == "__main__":
    part1()
    part2()
