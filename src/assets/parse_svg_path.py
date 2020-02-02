import pprint as pp
import string

chars = set(string.ascii_letters)
args = {
    "m": [float] * 2,
    "l": [float] * 2,
    "c": [float] * 6,
    # "a": [float] * 3 + [lambda s: bool(int(s))] * 2 + [float] * 2,
    "a": [float] * 3 + [int] * 2 + [float] * 2,
    "z": [],
    "h": [float],
    "v": [float],
}

x0, y0 = -203.24047, -98.667118
# x0, y0 = 0, 0


def tokenize(s):
    return s.strip().replace(",", " ").split()


def parse(tokens):
    i = 0
    while i < len(tokens):
        t = tokens[i]
        p = args[t]
        i += 1
        while i < len(tokens) and tokens[i] not in chars:
            yield (t, list(conv(a) for conv, a in zip(p, tokens[i : i + len(p)])))
            i += len(p)
        if t == "z":
            yield ("z", [])


def absolute(moves):
    x, y = x0, y0
    sx, sy = None, None
    for t, args in moves:
        if t == "m" or t == "l":
            dx, dy = args
            x += dx
            y += dy
            if sx is None:
                sx, sy = x, y
            yield (t.upper(), [x, y])
        elif t == "a":
            *prev, dx, dy = args
            x += dx
            y += dy
            yield (t.upper(), [*prev, x, y])
        elif t == "c":
            dx1, dy1, dx2, dy2, dx, dy = args
            yield (t.upper(), [x + dx1, y + dy1, x + dx2, y + dy2, x + dx, y + dy])
            x += dx
            y += dy
        elif t == "h":
            dx, = args
            x += dx
            yield ("L", [x, y])
        elif t == "v":
            dy, = args
            y += dy
            yield ("L", [x, y])
        elif t == "z":
            x, y = sx, sy
            sx, sy = None, None
            yield (t.upper(), [])
        else:
            raise ValueError("unimplemented {}".format(t))


def show(moves):
    return "".join("{}{}".format(t, ",".join(map(str, args))) for t, args in moves)


def elm_items(moves):
    pt = "({:.2f}, {:.2f})"
    for t, args in moves:
        if t == "M" or t == "L":
            fmt = [pt]
        elif t == "C":
            fmt = [pt] * 3
        elif t == "A":
            fmt = [pt] + ["{}"] * 3 + [pt]
        elif t == "Z":
            fmt = []
        yield " ".join(["{}", *fmt]).format(t, *args)


def elm(moves):
    return "[ {}\n]".format("\n, ".join(elm_items(moves)))


print(show(absolute(parse(tokenize(input())))))
