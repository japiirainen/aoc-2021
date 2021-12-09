lines = [line.strip() for line in open('09.txt').readlines()]

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

print(res)
