

def get_primes(limit=50):
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
    print prime_factors(600851475143)
