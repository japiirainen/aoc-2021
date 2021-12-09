q = [int(x) for x in open('01.txt')]
p1 = sum(y > x for x, y in zip(q, q[1:]))
print(p1)
