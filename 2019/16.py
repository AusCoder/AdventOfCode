import time
import math
import itertools
from collections import deque
from pathlib import Path


def read_input():
    return [int(x) for x in Path("input/16.txt").read_text().strip()]


def gen_pattern(idx):
    g = _gen_base_pattern(idx)
    next(g)
    yield from g
    while True:
        yield from _gen_base_pattern(idx)


def _gen_base_pattern(idx):
    for x in [0, 1, 0, -1]:
        n = idx + 1
        while n > 0:
            yield x
            n -= 1


def run_fft(signal):
    signal = [
        sum(x * p for x, p in zip(signal, gen_pattern(idx)))
        for idx, _ in enumerate(signal)
    ]
    return [abs(x) % 10 for x in signal]


def part1():
    signal = read_input()

    for _ in range(100):
        signal = run_fft(signal)

    print("".join(str(x) for x in signal)[:8])


def run_fft_truncated(truncated_signal):
    signal = deque()
    s = 0
    while True:
        try:
            v = truncated_signal.pop()
        except IndexError:
            break
        s += v
        signal.appendleft(abs(s) % 10)
    return signal


def part2():
    # for large idx, this is just a sequence of running sums
    base_signal = read_input()
    idx = int("".join(str(x) for x in base_signal[:7]))

    assert idx + idx >= len(base_signal) * 10000

    signal = base_signal * 10000
    truncated_signal = deque(signal[idx:])
    for _ in range(100):
        truncated_signal = run_fft_truncated(truncated_signal)
    print("".join(str(x) for x in list(truncated_signal)[:8]))


if __name__ == "__main__":
    # part1()
    part2()
