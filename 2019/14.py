import re
import math
import operator
from collections import defaultdict, deque
from pathlib import Path


def gen_recipe(lines):
    ingredient_regex = re.compile(r"(\d+ [A-Z]+),?\s*(.*)")
    result_regex = re.compile(r"=> (\d+ [A-Z]+)")

    for line in lines:
        requirements = []
        while True:
            m = ingredient_regex.match(line)
            if m:
                ing, line = m.groups()
                quant, name = ing.split()
                requirements.append((int(quant), name))
                continue
            m = result_regex.match(line)
            if m:
                (res,) = m.groups()
                quant, name = res.split()
                result = (int(quant), name)
                break
            raise RuntimeError(line)
        yield requirements, result


def calculate_rank(requirements, name):
    if name == "ORE":
        return 0
    return sum(calculate_rank(requirements, n) for _, n in requirements[name]) + 1


def amounts_required_for_fuel(requirements, amounts_produced):
    fuel_requirements = {name: count for count, name in requirements["FUEL"]}
    excesses = defaultdict(int)
    while len(fuel_requirements) > 1:
        current_ing, _ = max(
            [(n, calculate_rank(requirements, n)) for n in fuel_requirements],
            key=operator.itemgetter(1),
        )
        amount_required = fuel_requirements.pop(current_ing)
        quant_multiplier = math.ceil(amount_required / amounts_produced[current_ing])

        excess = amounts_produced[current_ing] * quant_multiplier - amount_required
        excesses[current_ing] += excess

        for quant, name in requirements[current_ing]:
            amount_produced = quant * quant_multiplier

            if name in fuel_requirements:
                fuel_requirements[name] += amount_produced
            else:
                fuel_requirements[name] = amount_produced
    return fuel_requirements, excesses


def part1():
    lines = Path("input/14.txt").read_text().strip().split("\n")

    recipe = list(gen_recipe(lines))
    amounts_produced = {ing: c for _, (c, ing) in recipe}
    requirements = {ing: req for req, (_, ing) in recipe}

    fuel_requirements, _ = amounts_required_for_fuel(requirements, amounts_produced)

    print(fuel_requirements)


def produce_from_stockpile(
    stockpile, requirements, amounts_produced, ore_required_for_one_fuel
):
    producing_stack = deque()
    produced_multiple = 1000000

    while True:
        # This multiple scaling is very imprecise, I had to use some trial and error
        if (
            produced_multiple > 1
            and (stockpile["ORE"] / (produced_multiple * ore_required_for_one_fuel))
            < 100
        ):
            produced_multiple //= 10

        try:
            currently_producing = producing_stack.pop()
        except IndexError:
            currently_producing = "FUEL"
            print(stockpile)

        if currently_producing == "ORE":
            break

        is_enough_ingredients = all(
            stockpile[name] >= produced_multiple * quant
            for quant, name in requirements[currently_producing]
        )

        if is_enough_ingredients:
            for quant, name in requirements[currently_producing]:
                stockpile[name] -= produced_multiple * quant
            stockpile[currently_producing] += (
                produced_multiple * amounts_produced[currently_producing]
            )
            continue

        producing_stack.append(currently_producing)
        for quant, name in requirements[currently_producing]:
            if stockpile[name] < produced_multiple * quant:
                producing_stack.append(name)


def part2():
    lines = Path("input/14.txt").read_text().strip().split("\n")

    recipe = list(gen_recipe(lines))
    amounts_produced = {ing: c for _, (c, ing) in recipe}
    requirements = {ing: req for req, (_, ing) in recipe}

    fuel_requirements, _ = amounts_required_for_fuel(requirements, amounts_produced)
    ore_required_for_one_fuel = fuel_requirements["ORE"]

    stockpile = {"ORE": 1000000000000}
    for name in requirements:
        stockpile[name] = 0

    produce_from_stockpile(
        stockpile, requirements, amounts_produced, ore_required_for_one_fuel
    )

    print(stockpile["FUEL"])


def amounts_required_for_fuel(requirements, amounts_produced):
    fuel_requirements = {name: count for count, name in requirements["FUEL"]}
    excesses = defaultdict(int)
    while len(fuel_requirements) > 1:
        current_ing, _ = max(
            [(n, calculate_rank(requirements, n)) for n in fuel_requirements],
            key=operator.itemgetter(1),
        )
        amount_required = fuel_requirements.pop(current_ing)
        quant_multiplier = math.ceil(amount_required / amounts_produced[current_ing])

        excess = amounts_produced[current_ing] * quant_multiplier - amount_required
        excesses[current_ing] += excess

        for quant, name in requirements[current_ing]:
            amount_produced = quant * quant_multiplier

            if name in fuel_requirements:
                fuel_requirements[name] += amount_produced
            else:
                fuel_requirements[name] = amount_produced
    return fuel_requirements, excesses


if __name__ == "__main__":
    # part1()
    part2()
