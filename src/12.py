lines = [line.strip() for line in open("12.txt").read().splitlines()]


def make_edges(lines):
    edges = {}
    for line in lines:
        a, b = line.split('-')
        edges[a] = edges.get(a, []) + [b]
        edges[b] = edges.get(b, []) + [a]
    return edges

def part1(node, edges, visited=set()):
    if node == 'end':
        return 1
    total = 0
    for next in edges[node]:
        if next in visited: continue
        c = part1(next, edges, visited | {node} if node == node.lower() else visited)
        total += c
    return total

def part2(node, edges, visited=set(), doubled=False):
    if node == 'end': return 1
    total = 0
    for next in edges[node]:
        if next == 'start': continue
        if next in visited and doubled: continue
        if next in visited:
            total += part2(next, edges, visited | {node} if node == node.lower() else visited, True)
        else:
            total += part2(next, edges, visited | {node} if node == node.lower() else visited, doubled)
    return total


edges = make_edges(lines)

print(part1('start', edges))
print(part2('start', edges))
