lines = open('10.txt').read().splitlines()


def part1():
    tokens = {'{': '}', '[': ']', '(': ')', '<': '>'}
    points = {')': 3, ']': 57, '}': 1197, '>': 25137}
    p1 = 0
    for line in lines:
        q = []
        for char in line:
            if char in tokens:
                q.append(tokens[char])
            elif char == q[-1]:
                q.pop()
            else:
                p1 += points[char]
                break
    return p1


def part2():
    tokens = {'{': '}', '[': ']', '(': ')', '<': '>'}
    points = {')': 1, ']': 2, '}': 3, '>': 4}
    scores = []
    for line in lines:
        q = []
        for char in line:
            if char in tokens:
                q.append(tokens[char])
            elif char == q[-1]:
                q.pop()
            else:
                break
        else:
            score = 0
            for tok in q[::-1]:
                score *= 5
                score += points[tok]
            scores.append(score)

    scores.sort()
    return scores[len(scores) // 2]


print(part1())
print(part2())
