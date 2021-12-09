q = [int(x) for x in open('01.txt')]
r = [x + y + z for x, y, z in zip(q, q[1:], q[2:])]
p2 = sum(y > x for x, y in zip(r, r[1:]))
print(p2)
