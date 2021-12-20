lines = open("20.txt").readlines()
enhance = [char == '#' for char in lines[0]]
grid = [[char == '#' for char in row] for row in lines[2:]]

def step(grid):
    ng = []
    for row in grid:
        ng.append([0] * 10 + row + [0] * 10)
    for _ in range(10):
        ng.append([0] * (len(grid[0]) + 20))
        ng.insert(0, [0] * (len(grid[0]) + 20))
    ngc = [[0] * (len(ng[0]) - 2) for _ in range(len(ng) - 2)]
    for i in range(1, len(ng) - 1):
        for j in range(1, len(ng[i]) - 1):
            v = [ng[y][x] for y in [i-1,i,i+1] for x in [j-1,j,j+1]]
            d = int("".join(map(str, map(int, v))), 2)
            ngc[i-1][j-1] = enhance[d]
    return ngc

def remove_pad(grid):
    grid = grid[16:-16]
    grid = [row[16:-16] for row in grid]
    return grid

def part1(grid):
    for _ in range(2):
        grid = step(grid)
    grid = remove_pad(grid)
    return sum(map(sum, grid))

def part2(grid):
    for _ in range(25):
        for _ in range(2):
            grid = step(grid)
        grid = remove_pad(grid)
    return sum(map(sum, grid))

print(part1(grid))
print(part2(grid))
