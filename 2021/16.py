pyint = int
def int(x, y = 10):
    return pyint("".join(x), y)

k = list("".join(bin(int(c, 16))[2:].zfill(4) for c in open('16.txt').read().strip()))

def parse(k):
    version = int(k[:3], 2)
    k[:] = k[3:]
    typeid = int(k[:3], 2)
    k[:] = k[3:]
    if typeid == 4:
        data = []
        while True:
            cont = k.pop(0)
            data += k[:4]
            k[:] = k[4:]
            if cont == "0": break
        data = int(data, 2)
        return (version, typeid, data)
    else:
        packets = []
        if k.pop(0) == "0":
            length = int(k[:15], 2)
            k[:] = k[15:]
            d = k[:length]
            k[:] = k[length:]
            while d:
                packets.append(parse(d))
        else:
            num = int(k[:11], 2)
            k[:] = k[11:]
            for _ in range(num):
                packets.append(parse(k))
        return (version, typeid, packets)

def version_sum(k):
    t = k[0]
    if k[1] == 4:
        return t
    else:
        return t + sum(map(version_sum, k[2]))

def eval(k):
    if k[1] == 0:
        return sum(map(eval, k[2]))
    if k[1] == 1:
        t = 1
        for x in k[2]:
            t *= eval(x)
        return t
    if k[1] == 2:
        return min(map(eval, k[2]))
    if k[1] == 3:
        return max(map(eval, k[2]))
    if k[1] == 4:
        return k[2]
    if k[1] == 5:
        return eval(k[2][0]) > eval(k[2][1])
    if k[1] == 6:
        return eval(k[2][0]) < eval(k[2][1])
    if k[1] == 7:
        return eval(k[2][0]) == eval(k[2][1])


p = parse(k)
print(version_sum(p))
print(eval(p))
