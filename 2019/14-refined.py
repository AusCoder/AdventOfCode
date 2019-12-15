import re
import math
import operator
from collections import defaultdict, deque
from pathlib import Path


def gen_recipe(lines):
    ingredient_regex = re.compile(r"(\d+ [A-Z]+),?\s*(.*)")
    result_regex = re.compile(r"=> (\d+ [A-Z]+)")

    for line in lines:
        ingredients = []
        while True:
            m = ingredient_regex.match(line)
            if m:
                ing, line = m.groups()
                quant, name = ing.split()
                ingredients.append((int(quant), name))
                continue
            m = result_regex.match(line)
            if m:
                (res,) = m.groups()
                quant, name = res.split()
                result = (int(quant), name)
                break
            raise RuntimeError(line)
        yield ingredients, result


def calculate_top_order(ratios, name):
    if name == "ORE":
        return 0
    return sum(calculate_top_order(ratios, n) for _, n in ratios[name]) + 1


def calculate_ore_for_fuel(ratios, amounts_produced, amount_fuel):
    requirements = defaultdict(int)
    requirements["FUEL"] = amount_fuel
    stockpile = defaultdict(int)

    while [x for x in requirements if x != "ORE"]:
        current_ing, _ = max(
            [(n, calculate_top_order(ratios, n)) for n in requirements],
            key=operator.itemgetter(1),
        )
        amount_required = requirements.pop(current_ing)
        amount_required -= stockpile[current_ing]
        stockpile[current_ing] = 0

        quant_multiplier = math.ceil(amount_required / amounts_produced[current_ing])
        excess = amounts_produced[current_ing] * quant_multiplier - amount_required
        stockpile[current_ing] += excess

        for quant, name in ratios[current_ing]:
            amount_ingredient_required = quant * quant_multiplier
            requirements[name] += amount_ingredient_required
    return requirements["ORE"]


def part1():
    lines = Path("input/14.txt").read_text().strip().split("\n")

    recipe = list(gen_recipe(lines))
    amounts_produced = {ing: c for _, (c, ing) in recipe}
    ratios = {ing: req for req, (_, ing) in recipe}

    required_ore = calculate_ore_for_fuel(ratios, amounts_produced, 1)
    print(required_ore)


def find_max_fuel(ratios, amounts_produced):
    total_ore = 1000000000000

    possible_fuel = 1
    prev_possible_fuel = 1
    while True:
        required_ore = calculate_ore_for_fuel(ratios, amounts_produced, possible_fuel)
        if required_ore > total_ore:
            break
        prev_possible_fuel = possible_fuel
        possible_fuel *= 2

    possible_fuels = list(range(prev_possible_fuel, possible_fuel))
    start = 0
    stop = len(possible_fuels) - 1

    while start <= stop:
        mid = start + (stop - start) // 2
        possible_fuel = possible_fuels[mid]
        required_ore = calculate_ore_for_fuel(ratios, amounts_produced, possible_fuel)

        if required_ore < total_ore:
            start = mid + 1
        elif required_ore > total_ore:
            stop = mid - 1
        else:
            break

    if required_ore > total_ore:
        possible_fuel = possible_fuels[mid - 1]
    return possible_fuel


def part2():
    lines = Path("input/14.txt").read_text().strip().split("\n")

    recipe = list(gen_recipe(lines))
    amounts_produced = {ing: c for _, (c, ing) in recipe}
    ratios = {ing: req for req, (_, ing) in recipe}
    max_fuel = find_max_fuel(ratios, amounts_produced)
    print(max_fuel)


if __name__ == "__main__":
    # part1()
    part2()
