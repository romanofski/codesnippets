

def is_palindrome(mapping):
    """ Returns true if mapping is a palindrome

    >>> is_palindrome('otto')
    True
    >>> is_palindrome('ottof')
    False
    """
    mapping = list(mapping)
    for x in mapping:
        y = mapping.pop()
        if x == y:
            return True
        else:
            break
    return False


if __name__ == '__main__':
    import doctest
    doctest.testmod()
