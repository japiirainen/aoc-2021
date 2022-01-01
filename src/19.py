from collections import Counter


def directions():
    mps = [(0, 1), (1, 1), (2, 1), (0, -1), (1, -1), (2, -1)]
    dirs = []
    for f1 in mps:
        for f2 in mps:
            for f3 in mps:
                if len(set([f1[0], f2[0], f3[0]])) == 3:
                    dirs.append((f1, f2, f3))
    return dirs


def remap(d, coords):
    newc = []
    for v in coords:
        newc.append(
            (
                d[0][1] * v[d[0][0]],
                d[1][1] * v[d[1][0]],
                d[2][1] * v[d[2][0]],
            )
        )
    return newc


def dv(p1, p2):
    return (p1[0] - p2[0], p1[1] - p2[1], p1[2] - p2[2])


def find_match(s1, s2):
    for d in directions():
        newc = remap(d, s2)
        deltas = Counter()
        for p1 in s1:
            for p2 in newc:
                deltas[dv(p1, p2)] += 1
        for k, v in deltas.items():
            if v < 12:
                continue
            newc2 = set()
            for p in newc:
                newc2.add((p[0] + k[0], p[1] + k[1], p[2] + k[2]))
            return True, k, newc2
    return False, None, None


def solve(v):
    chunks = v.split("\n\n")
    scanners = []
    for c in chunks:
        lines = c.strip().split("\n")
        coords = []
        for line in lines[1:]:
            coords.append(tuple(map(int, line.split(","))))
        scanners.append(coords)
    pts = set(scanners[0])
    pos = [(0, 0, 0)]
    q = list(enumerate(scanners))
    while q:
        q2 = []
        for i, sc in q:
            ok, scpos, extra = find_match(pts, sc)
            if ok:
                pts |= extra
                pos.append(scpos)
            else:
                q2.append((i, sc))
        q = q2
    best = 0
    for p1 in pos:
        for p2 in pos:
            best = max(best, manhattan_distance(p1, p2))
    return len(pts), best


def manhattan_distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])


with open("19.txt") as f:
    data = f.read()
    lenpts, best = solve(data)
    print(lenpts)
    print(best)
