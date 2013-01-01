import timeit


def get_primes(limit=50):
    """ prime number generator, sieve of Eratosthenes

    >>> get_primes(5)
    [2, 3, 5]
    >>> get_primes(8)
    [2, 3, 5, 7]
    >>> get_primes(20)
    [2, 3, 5, 7, 11, 13, 17, 19]
    """
    primes = [2, 3]
    for x in xrange(4, limit + 1):
        if x % 2 == 0 or x % 3 == 0:
            continue
        primes.append(x)
    return primes


def get_primes_old(limit=50):
    """ prime number generator, sieve of Eratosthenes

    >>> get_primes(5)
    [2, 3, 5]
    >>> get_primes(8)
    [2, 3, 5, 7]
    """
    potentials = []
    primes = []
    for i in xrange(2, limit + 1):
        potentials.append(i)
    potentials.reverse()
    while potentials:
        x = potentials.pop()
        primes.append(x)
        new = []
        for p in potentials:
            if p % x != 0:
                new.append(p)
        potentials = new

    return primes


def prime_factors(number, solution=None):
    """ Crappy prime factor generator.

    >>> prime_factors(12)
    [2, 2, 3]
    >>> prime_factors(13195)
    [5, 7, 13, 29]
    >>> prime_factors(330)
    [2, 3, 5, 11]
    """
    if solution is None:
        solution = []

    for i in get_primes(50000):
        rest = number % i
        if rest > 0:
            continue
        else:
            solution.append(i)
            solution = prime_factors(number / i, solution=solution)
            break
    return solution

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    # print timeit.Timer("get_primes_old()", "from __main__ import get_primes_old").timeit()
    # print timeit.Timer("get_primes()", "from __main__ import get_primes").timeit()
    fact = prime_factors(600851475143)
    print fact
    print reduce(lambda x, y: x * y, fact) == 600851475143
