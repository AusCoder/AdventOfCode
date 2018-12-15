from collections import deque
from typing import List, Deque, Tuple


def part1(num_recipes):
    recipes = deque([3, 7])
    i1 = 0
    i2 = 1
    while len(recipes) <= num_recipes + 10:
        score1 = recipes[i1]
        score2 = recipes[i2]
        s = score1 + score2
        assert s >= 0 and s < 19
        if s < 10:
            recipes.append(s)
        else:
            recipes.append(s // 10)
            recipes.append(s % 10)
        i1 = (i1 + 1 + score1) % len(recipes)
        i2 = (i2 + 1 + score2) % len(recipes)

    return ''.join([str(recipes[num_recipes + i]) for i in range(10)])


# assert part1(9) == '5158916779'
# assert part1(5) == '0124515891'
# assert part1(18) == '9251071085'
# assert part1(2018) == '5941429882'

# print(part1(147061))

def is_match(scores: List[int], recipes: List[int], start_idx: int) -> Tuple[bool, int]:
    idx = start_idx
    is_match = False
    while idx < len(recipes) - len(scores) + 1:
        is_match = all([x == y for x, y in zip(scores, recipes[idx:])])
        if is_match:
            break
        idx += 1
    return is_match, idx

def part2(scores):
    scores = [int(x) for x in scores]
    recipes = [3, 7]
    i1 = 0
    i2 = 1

    start_idx = 0
    while True:
        is_match_val, end_idx = is_match(scores, recipes, start_idx)
        start_idx = end_idx
        if is_match_val:
            break

        score1 = recipes[i1]
        score2 = recipes[i2]
        s = score1 + score2

        if s < 10:
            recipes.append(s)
        else:
            recipes.append(s // 10)
            recipes.append(s % 10)
        i1 = (i1 + 1 + score1) % len(recipes)
        i2 = (i2 + 1 + score2) % len(recipes)

    return start_idx


# print('num recipes: {}'.format(part2('51589')))
# assert part2('51589') == 9
# assert part2('01245') == 5
# assert part2('92510') == 18
# assert part2('59414') == 2018

print(part2('147061'))
