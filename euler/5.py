import doctest
import itertools


def search(limit):
    """ Searches for the smallest number evenly divisable from 1..10.

    >>> search(10)
    2520
    >>> search(20)
    2323
    """
    backwards = range(1, limit + 1)
    # the number has to be divisable by 20, which means we step all 20
    # and start with 19, since we know already that the number every 20s
    # step is divisable by twenty
    x = limit
    y = backwards[0]
    while y != limit:
        for y in backwards:
            rest = x % y
            if rest != 0:
                print (x, y, rest)
                break
            if y == 20:
                print x
        x = x * y


if __name__ == "__main__":
    # doctest.testmod()
    search(20)
