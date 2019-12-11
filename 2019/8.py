import itertools
from pathlib import Path


WIDTH = 25
HEIGHT = 6


def gen_layers():
    encoded_image = [int(x) for x in Path("input/8.txt").read_text().strip()]
    layer_stride = WIDTH * HEIGHT
    yield from (
        encoded_image[n : n + layer_stride]
        for n in range(0, len(encoded_image), layer_stride)
    )


def part1():
    layers = gen_layers()
    fewest_0_digits = min(layers, key=lambda l: l.count(0))
    print(fewest_0_digits.count(1) * fewest_0_digits.count(2))


def part2():
    layers = gen_layers()
    img = [None] * WIDTH * HEIGHT
    for layer in layers:
        for i in range(WIDTH * HEIGHT):
            if img[i] is None and layer[i] != 2:
                img[i] = layer[i]
    rows = (img[n : n + WIDTH] for n in range(0, WIDTH * HEIGHT, WIDTH))
    for row in rows:
        for x in row:
            print("*" if x else " ", end="")
        print()


if __name__ == "__main__":
    # part1()
    part2()
