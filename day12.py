import numpy as np

# strinitst = '#..#.#..##......###...###'
strinitst = '##.#.#.##..#....######..#..#...#.#..#.#.#..###.#.#.#..#..###.##.#..#.##.##.#.####..##...##..#..##.#.'

def r(ststr, padding=0):
    st = np.zeros(len(ststr) + padding * 2, dtype=int)
    for i in range(len(ststr)):
        if ststr[i] == '#':
            st[i + padding] = 1
    return st

def read_updates():
    updates = []
    with open('input/day12.txt') as fh:
        for line in fh.readlines():
            sp = line.split()
            if sp[2] == '#':
                st1 = r(sp[0])
                updates.append(st1)
    return updates

def part1(ngen):
    updates = read_updates()

    orig = 2 * ngen
    st = r(strinitst, padding=orig)
    nextst = np.array(st, copy=True)

    totalps = 0
    for _ in range(ngen):
        for i in range(st.shape[0] - 5):
            found = False
            for u in updates:
                if np.all(u == st[i:i+5]):
                    nextst[i+2] = 1
                    found = True
            if not found:
                nextst[i + 2] = 0

            _c = i + 5
        assert _c == st.shape[0] - 1
        st[:] = nextst

    t = 0
    for i in range(st.shape[0]):
        if st[i] == 1:
            t += i - orig
    return t
    # print('ngen: {}. ans: {}'.format(ngen, t))


def part2(ngen):
    updates = read_updates()

    orig = ngen
    st = r(strinitst, padding=orig)
    nextst = np.array(st, copy=True)

    prev = None
    for gennum in range(ngen):
        for i in range(st.shape[0] - 5):
            found = False
            for u in updates:
                if np.all(u == st[i:i+5]):
                    nextst[i+2] = 1
                    found = True
            if not found:
                nextst[i + 2] = 0

            _c = i + 5
        assert _c == st.shape[0] - 1
        st[:] = nextst

        t = 0
        for i in range(st.shape[0]):
            if st[i] == 1:
                t += i - orig
        if gennum > 0:
            print('gennum: {}. total: {}. diff: {}'.format(gennum + 1, t, t - prev))

        prev = t



part2(1000)
# increasing by 38 after about generation 100
# (50000000000 - 100) * 38 + 4184
