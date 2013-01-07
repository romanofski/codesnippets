import doctest


def search(limit):
    """ Searches for the smallest number evenly divisable from 1..10.

    >>> search(10)
    2520
    >>> search(20)
    232792560
    """
    backwards = range(1, limit + 1)
    # the number has to be divisable by 20, which means we step all 20
    # and start with 19, since we know already that the number every 20s
    # step is divisable by twenty
    x = limit
    y = backwards[0]
    divisable = []
    while not divisable:
        for y in backwards:
            rest = x % y
            if rest != 0:
                divisable.append((x, y, rest))
        if divisable:
            x = x + (divisable[0][1] - divisable[0][2])
            if x % 500000 == 0:
                print x
            divisable = []
        else:
            break
    print x


if __name__ == "__main__":
    #doctest.testmod()
    search(20)
