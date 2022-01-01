import heapq

_risk = [list(map(int, line.strip())) for line in open("15.txt").readlines()]

def lrp(risk):
    vis = [[0] * len(r) for r in risk]
    ps = [(0, 0, 0)]
    res = 0
    while res == 0:
        rc, x, y = heapq.heappop(ps)
        if vis[x][y] == 1:
            continue
        if x == len(risk) - 1 and y == len(risk[x]) - 1:
            res = rc
        vis[x][y] = 1
        for i, j in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]:
            if not len(risk) > i >= 0 <= j < len(risk[i]):
                continue
            if vis[i][j]:
                continue
            heapq.heappush(ps, (rc + risk[i][j], i, j))
    print(res)


def p1(risk):
    lrp(risk)


def wrap(x):
    return (x - 1) % 9 + 1


def p2(risk):
    orig = risk
    r = len(orig)
    c = len(orig[0])
    risk = [[0] * len(r) * 5 for r in risk * 5]
    for i in range(len(risk)):
        for j in range(len(risk[i])):
            risk[i][j] = wrap(orig[i % r][j % c] + i // r + j // c)

    lrp(risk)


p1(_risk)
p2(_risk)
