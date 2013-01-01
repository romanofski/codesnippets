# Euler 2


def fibonacci(limit, j=0, k=1, l=0):
    while l < limit:
        j = k
        k = l
        l = j + k
        if l % 2 == 0:
            yield l
        else:
            continue


if __name__ == '__main__':
    foo = [x for x in fibonacci(4000000) if x % 2 == 0]
    print foo
    print sum(foo)
