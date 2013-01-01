

def get_primes():
    return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]


def prime_factors(number, children=None, solution=None):
    """ Crappy prime factor generator.

    >>> prime_factors(12)
    [2, 2, 3]
    >>> prime_factors(13195)
    [5, 7, 13, 29]
    """
    if children is None:
        children = []
    if solution is None:
        solution = []

    for i in get_primes():
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
