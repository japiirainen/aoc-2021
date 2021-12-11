grid = [list(map(int, line))
        for line in open('11-sample.txt').read().splitlines()]


r1 = 0


def flash(grid, row, col):
    global r1
    r1 += 1
    for i in range(row - 1, row + 2):
        for j in range(col - 1, col + 2):
            if i == row and j == col:
                continue
            if 0 <= i < len(grid) and 0 <= j < len(grid[i]):
                grid[i][j] += 1
                if grid[i][j] == 10:
                    flash(grid, i, j)
                    grid[i][j] += 1


def step(grid):
    for row in range(len(grid)):
        for col in range(len(grid[row])):
            grid[row][col] += 1
            if grid[row][col] == 10:
                flash(grid, row, col)
                grid[row][col] += 1
    for row in range(len(grid)):
        for col in range(len(grid[row])):
            if grid[row][col] > 9:
                grid[row][col] = 0


for _ in range(100):
    step(grid)


print(r1)
