import re
import math
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Deque, Any


@dataclass
class DealNewStack:
    pass


@dataclass
class DealWithIncrement:
    increment: int


@dataclass
class Cut:
    value: int


def read_input():
    return Path("input/22.txt").read_text().strip().split("\n")


def gen_commands(lines):
    for line in lines:
        incr_match = re.match(r"deal with increment (\d+)", line)
        cut_match = re.match(r"cut (-?\d+)", line)
        if line == "deal into new stack":
            yield DealNewStack()
        elif incr_match:
            incr = int(incr_match.group(1))
            yield DealWithIncrement(incr)
        elif cut_match:
            val = int(cut_match.group(1))
            yield Cut(val)
        else:
            raise RuntimeError(line)


def deal_with_incr(cards, incr):
    res = {}
    for i, c in enumerate(cards):
        dst_idx = (incr * i) % len(cards)
        res[dst_idx] = c
    return [res[i] for i in range(len(res))]


def apply_commands_list(commands, cards):
    for command in commands:
        if isinstance(command, DealNewStack):
            cards = list(reversed(cards))
        elif isinstance(command, DealWithIncrement):
            cards = deal_with_incr(cards, command.increment)
        elif isinstance(command, Cut):
            cards = cards[command.value :] + cards[: command.value]
        else:
            raise RuntimeError(command)
    return cards


def part1():
    commands = gen_commands(read_input())
    cards = list(range(10007))
    cards = apply_commands_list(commands, cards)
    print(cards.index(2019))


def apply_commands_idx(commands, len_cards, idx):
    """Commands are just modular arithmetic on the index:

    Cut(value=x):
        i -> (i - x) % len_cards
    DealWithIncrement(increment=x):
        i -> (i * x) % len_cards
    DealNewStack():
        i -> (-i - 1) % len_cards
    """
    for command in commands:
        if isinstance(command, DealNewStack):
            idx = (-idx - 1) % len_cards
        elif isinstance(command, DealWithIncrement):
            assert math.gcd(command.increment, len_cards) == 1
            idx = (command.increment * idx) % len_cards
        elif isinstance(command, Cut):
            idx = (idx - command.value) % len_cards
            assert math.gcd(command.value, len_cards) == 1
        else:
            raise RuntimeError(command)
    return idx


def mod_inverse(x, m):
    """Computes x^(-1) mod m"""
    inv, _ = extended_gcd(x, m)
    return inv


def extended_gcd(a, b):
    x, lastX = 0, 1
    y, lastY = 1, 0
    while b != 0:
        q = a // b
        a, b = b, a % b
        x, lastX = lastX - q * x, x
        y, lastY = lastY - q * y, y
    return lastX, lastY


def mod_power(x, a, m):
    """Computes x^a mod m"""
    r = 1
    x = x % m
    while a > 0:
        if a & 1:
            r = (r * x) % m
        a >>= 1
        x = (x * x) % m
    return r


def apply_inverse_deal_with_increment_then_cut_iterated(
    commands, iters, idx, len_cards
):
    """
    Applies the inverse of a deal then a cut, iterated `iters` times.

    A cut then a deal is a function like:
        i -> (i - x)y
    When iterated n times, this can be simplified to:
        i -> iy^n - xy ((y^n - 1) / (y - 1))
    """
    deal, cut = commands
    assert isinstance(deal, DealWithIncrement)
    assert isinstance(cut, Cut)

    inverse = mod_inverse(deal.increment, len_cards)
    incr_raised = mod_power(inverse, iters, len_cards)
    frac = ((incr_raised - 1) * mod_inverse(inverse - 1, len_cards)) % len_cards
    return (idx * incr_raised + cut.value * inverse * frac) % len_cards


def apply_inverse_commands_idx(commands, len_cards, idx):
    """Applies the inverse of each command.
    """
    for command in commands:
        if isinstance(command, DealNewStack):
            idx = (-idx - 1) % len_cards
        elif isinstance(command, DealWithIncrement):
            inverse = mod_inverse(command.increment, len_cards)
            idx = (inverse * idx) % len_cards
        elif isinstance(command, Cut):
            idx = (idx + command.value) % len_cards
        else:
            raise RuntimeError(command)
    return idx


def condense_commands(commands, len_cards):
    while True:
        try:
            commands = _condense_pass(commands, len_cards)
        except ValueError:
            break
    return commands


def _condense_pass(commands, len_cards):
    """Commands can be moved passed each other and combined by working out
    the appropriate modular arithmetic.
    """
    x, y, *rest = commands
    if isinstance(x, Cut) and isinstance(y, DealWithIncrement):
        cut_val = (y.increment * x.value) % len_cards
        return [DealWithIncrement(y.increment), Cut(cut_val), *rest]
    elif isinstance(x, Cut) and isinstance(y, DealNewStack):
        cut_val = (-x.value) % len_cards
        return [DealNewStack(), Cut(cut_val), *rest]
    elif isinstance(x, DealWithIncrement) and isinstance(y, DealNewStack):
        cut_val = (mod_inverse(x.increment, len_cards) - 1) % len_cards
        return [DealNewStack(), Cut(cut_val), DealWithIncrement(x.increment), *rest]
    elif isinstance(x, Cut) and isinstance(y, Cut):
        return [
            Cut((x.value + y.value) % len_cards),
            *rest,
        ]
    elif isinstance(x, DealWithIncrement) and isinstance(y, DealWithIncrement):
        return [DealWithIncrement((x.increment * y.increment) % len_cards), *rest]
    elif isinstance(x, DealNewStack) and isinstance(y, DealNewStack):
        return rest
    else:
        return [x, *_condense_pass([y, *rest], len_cards)]


def part2():
    lines = read_input()
    commands = list(gen_commands(lines))
    track_idx = 2020
    len_cards = 119315717514047
    iters = 101741582076661

    commands = condense_commands(commands, len_cards)
    print(f"Command list is equivalent to: {commands}")
    print(
        apply_inverse_deal_with_increment_then_cut_iterated(
            commands, iters, track_idx, len_cards
        )
    )


if __name__ == "__main__":
    # part1()
    part2()
