#!/usr/bin/python3
from collections import Counter


s, rules = open('14.txt').read().split('\n\n')

r = {}
for line in rules.splitlines():
    start, end = line.split(' -> ')
    r[start] = end

c1 = Counter()
for i in range(len(s)-1):
    c1[s[i]+s[i+1]] += 1

for t in range(41):
    if t in [10, 40]:
        cf = Counter()
        for k in c1:
            cf[k[0]] += c1[k]
        cf[s[-1]] += 1
        print(max(cf.values())-min(cf.values()))

    # If AB->R, then AB becomes (AR, RB)
    c2 = Counter()
    for k in c1:
        c2[k[0]+r[k]] += c1[k]
        c2[r[k]+k[1]] += c1[k]
    c1 = c2
