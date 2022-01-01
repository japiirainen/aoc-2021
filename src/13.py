data=open('13.txt').read()

ds, ins = data.split("\n\n")

def initial_dots(ds):
    dots = set()
    for line in ds.splitlines():
        x, y = map(int, line.split(','))
        dots.add((x, y))
    return dots

def part1(ds, ins):
    dots = initial_dots(ds)
    for line in ins.splitlines():
        line = line[11:]
        t, n = line.split('=')
        n = int(n)
        if t == 'x':
            dots = {(x if x < n else n - (x - n), y) for x, y in dots}
        else:
            dots = {(x, y if y < n else n - (y - n)) for x, y in dots}
        # Break since we only want to do the first fold
        break
    return len(dots)

def part2(ds, ins):
    dots = initial_dots(ds)
    for line in ins.splitlines():
        line = line[11:]
        t, n = line.split('=')
        n = int(n)
        if t == 'x':
            dots = {(x if x < n else n - (x - n), y) for x, y in dots}
        else:
            dots = {(x, y if y < n else n - (y - n)) for x, y in dots}

    mx = max(x for x, _ in dots)
    my = max(y for _, y in dots)
    g = [[" "] * (mx + 1) for _ in range(my + 1)]
    for x, y in dots:
        g[y][x] = '#'

    for r in g:
        print("".join(r))


print(part1(ds, ins))
part2(ds, ins)
