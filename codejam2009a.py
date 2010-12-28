LARGEINPUT = 'A-large-practice.in'


def parse_doc(fh):
    """Parses a input file with alien language.
    """
    words = tests = []
    i = 1
    length, wordamt, testamt = fh.readline().split(' ')
    wordamt = int(wordamt)
    testamt = int(testamt)
    for line in fh:
        if i <= wordamt:
            words.append(line.strip('\n'))
        elif i >= wordamt and i <= testamt:
            tests.append(line.strip('\n'))
        i += 1
    return (words, tests)


def main():
    words, tests = parse_doc(open(LARGEINPUT, 'r'))


if __name__ == "__main__":
    main()
