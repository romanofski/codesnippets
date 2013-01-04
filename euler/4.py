

def is_palindrome(mapping):
    """ Returns true if mapping is a palindrome

    >>> is_palindrome('otto')
    True
    >>> is_palindrome('ottof')
    False
    >>> is_palindrome(91219)
    True
    >>> is_palindrome(9219)
    False
    """
    mapping = list(str(mapping))
    index = 0
    while mapping and index < len(mapping):
        x = mapping[index]
        y = mapping.pop()
        if x != y:
            return False
        index += 1
    return True


def bruteforce():
    x = 900
    while x <= 1000:
        y = 900
        while y <= 1000:
            if is_palindrome(x * y):
                print "{x} ({y})".format(x=(x, y), y=x * y)
            y += 1
        x += 1


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    bruteforce()
