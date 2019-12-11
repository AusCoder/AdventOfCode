import math
from pathlib import Path

def fuel_for_mass(m):
    return math.floor(m / 3) - 2

ns = [int(x) for x in Path("input/1.txt").read_text().split("\n") if x]

fr = (fuel_for_mass(x) for x in ns)
print("part 1:")
print(sum(fr))
print()

def fuel_for_mass_with_fuel(m):
    def gen_masses(m):
        x = fuel_for_mass(m)
        yield x
        while True:
            x = fuel_for_mass(x)
            if x <= 0:
                break
            yield x
    return sum(gen_masses(m))

fr = (fuel_for_mass_with_fuel(m) for m in ns)
print("part 2:")
print(sum(fr))
