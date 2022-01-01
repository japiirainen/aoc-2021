lines = open('22.txt').readlines()

def grange(q):
    x, y = map(int, q.split('..'))
    return range(max(x, -50), min(y, 50) + 1)

on = set()

for line in lines:
    instruction, data = line.split()
    xd, yd, zd = [x[2:] for x in data.split(',')]
    for x in grange(xd):
        for y in grange(yd):
            for z in grange(zd):
                if instruction == 'on':
                    on.add((x, y, z))
                else: on.discard((x, y, z))

print(len(on))
