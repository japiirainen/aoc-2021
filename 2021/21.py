data = open("21.txt").readlines()
p1i = int(data[0].split("Player 1 starting position: ")[1].strip()) - 1
p2i = int(data[1].split("Player 2 starting position: ")[1].strip()) - 1

rolls = 0


def roll():
    global rolls
    rolls += 1
    return rolls


def part1():
    p1 = p1i
    p2 = p2i
    s1 = 0
    s2 = 0
    while True:
        r1 = roll() + roll() + roll()
        p1 = (p1 + r1) % 10
        s1 += p1 + 1
        if s1 >= 1000:
            print(rolls * s2)
            break

        r2 = roll() + roll() + roll()
        p2 = (p2 + r2) % 10
        s2 += p2 + 1
        if s2 >= 1000:
            print(rolls * s1)
            break


def part2():
    p1 = p1i
    p2 = p2i
    dp = {}

    def d(p1, p2, s1, s2):
        if s1 >= 21:
            return (1, 0)
        if s2 >= 21:
            return (0, 1)
        if (p1, p2, s1, s2) in dp:
            return dp[(p1, p2, s1, s2)]
        ans = (0, 0)
        for d1 in [1, 2, 3]:
            for d2 in [1, 2, 3]:
                for d3 in [1, 2, 3]:
                    new_p1 = (p1 + d1 + d2 + d3) % 10
                    new_s1 = s1 + new_p1 + 1

                    x1, y1 = d(p2, new_p1, s2, new_s1)
                    ans = (ans[0] + y1, ans[1] + x1)
        dp[(p1, p2, s1, s2)] = ans
        return ans

    print(max(d(p1, p2, 0, 0)))


part1()
part2()
