lines = [line.strip() for line in open('02.txt').readlines()]


def part1(lines):
    h = d = 0
    for line in lines:
        dir, x = line.split(' ')
        x = int(x)
        if dir[0] == 'f':
            h += x
        elif dir[0] == 'd':
            d += x
        else:
            d -= x
    return h * d


def part2(lines):
    h = d = a = 0
    for line in lines:
        dir, x = line.split(' ')
        x = int(x)
        if dir[0] == 'f':
            h += x
            d += a * x
        elif dir[0] == 'd':
            a += x
        else:
            a -= x
    return h * d


print(part1(lines))
print(part2(lines))
