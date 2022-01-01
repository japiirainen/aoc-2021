import re

minx, maxx, miny, maxy = map(int, re.findall(r"-?\d+", open("17.txt").read()))

x_ps = []
for ix in range(0, maxx + 1):
    x = 0
    x_ps.append([0] + [x := x + dx for dx in range(ix, 0, -1)])

highest = -1
vs = set()
for iy in range(miny, -miny):
    posy = 0
    dy = iy
    topy = 0
    n = 0
    while posy >= miny:
        if posy <= maxy:
            for ix in range(0, maxx + 1):
                step = min(n, len(x_ps[ix]) - 1)
                if minx <= x_ps[ix][step] <= maxx:
                    highest = max(highest, topy)
                    vs.add((ix, iy))

        posy += dy
        if dy == 0:
            topy = posy
        dy -= 1
        n += 1

print(highest)
print(len(vs))
