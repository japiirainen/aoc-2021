q = [int(x) for x in open('01.txt')]

part1 = sum(y > x for x, y in zip(q, q[1:]))


def part2():
    zs = [x + y + z for x, y, z in zip(q, q[1:], q[2:])]
    return sum(y > x for x, y in zip(zs, zs[1:]))


print(part1)
print(part2())
