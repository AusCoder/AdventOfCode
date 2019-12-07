from collections import deque, defaultdict


# I saw a solution for the second part.
# Rotating is much faster than inserting.
def part1(numPlayers, maxMarble):
    m = deque([0])
    s = defaultdict(int)

    cur = 0
    curP = 1
    for curM in range(1, maxMarble + 1):
        if curM % 23 == 0:
            m.rotate(7)
            s[curP] += curM + m.pop()
            m.rotate(-1)
        else:
            m.rotate(-1)
            m.append(curM)

        curP += 1
        if curP > numPlayers:
            curP = 1

    print(max(s.values()))



part1(9, 25)
# part1(9, 25 * 100)

part1(10, 1618)
# part1(10, 1618 * 100)
part1(13, 7999)
part1(463, 71787 * 100)
