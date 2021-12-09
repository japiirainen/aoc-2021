from collections import deque
lines = [line.strip() for line in open('09.txt').readlines()]


def part1():
    res = 0
    for row in range(len(lines)):
        for col in range(len(lines[row])):
            neighbors = []
            for i, j in [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]:
                if 0 <= i < len(lines) and 0 <= j < len(lines[i]):
                    neighbors.append(int(lines[i][j]))

            curr = int(lines[row][col])
            if curr < min(neighbors):
                res += curr + 1
    return res


def part2():
    lines = [[int(x) for x in line.strip()]
             for line in open('09.txt').readlines()]

    g = [[0] * len(r) for r in lines]

    T = 1

    for r in range(len(lines)):
        for c in range(len(lines[r])):
            if lines[r][c] == 9 or g[r][c] != 0:
                continue
            vis = {(r, c)}
            q = deque()
            q.append((r, c))
            while q:
                r, c = q.popleft()
                g[r][c] = T
                for i, j in [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]:
                    if (i, j) in vis:
                        continue
                    if 0 <= i < len(lines) and 0 <= j < len(lines[i]):
                        if lines[i][j] != 9:
                            vis.add((i, j))
                            q.append((i, j))
            T += 1

    p = sum(g, [])
    c = [0] * T

    for x in p:
        c[x] += 1

    c = c[1:]
    c.sort()

    return c[-3] * c[-2] * c[-1]


print(part1())
print(part2())
