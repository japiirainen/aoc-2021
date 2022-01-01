grid = [[c for c in line.strip()] for line in open("25.txt").readlines()]


east = set()
south = set()

for i in range(len(grid)):
    for j in range(len(grid[i])):
        curr = grid[i][j]
        if curr == ">":
            east.add((i, j))
        elif curr == "v":
            south.add((i, j))


i = 0
while True:
    ne = set()
    ns = set()
    all = east | south
    for x, y in east:
        n = (x, (y + 1) % len(grid[0]))
        if n in all:
            ne.add((x, y))
        else:
            ne.add(n)
    east_eq = ne == east
    east = ne
    all = east | south
    for x, y in south:
        n = ((x + 1) % len(grid), y)
        if n in all:
            ns.add((x, y))
        else:
            ns.add(n)
    south_eq = ns == south
    if east_eq and south_eq:
        i += 1
        break
    south = ns
    i += 1


print(i)
