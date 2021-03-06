#!/usr/bin/python3
from collections import Counter

s, rules = open('14.txt').read().split('\n\n')

r = {}
for line in rules.splitlines():
    start, end = line.split(' -> ')
    r[start] = end

c1 = Counter([s[i]+s[i+1] for i in range(len(s)-1)])

for t in range(40):
    # If AB->R, then AB becomes (AR, RB)
    c2 = Counter()
    for k in c1:
        c2[k[0]+r[k]] += c1[k]
        c2[r[k]+k[1]] += c1[k]
    c1 = c2

    if t in [9, 39]:
        cf = Counter()
        for k in c1:
            cf[k[0]] += c1[k]
        # Always inc the last char, since we only add in the middle
        cf[s[-1]] += 1
        print(max(cf.values())-min(cf.values()))
