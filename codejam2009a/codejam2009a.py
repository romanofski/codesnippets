LARGEINPUT = 'A-large-practice.in'
import re


def parse_doc(fh):
    """Parses a input file with alien language.
    """
    words = []
    tests = []
    i = 1
    length, wordamt, testamt = fh.readline().split(' ')
    wordamt = int(wordamt)
    testamt = int(testamt)
    for line in fh:
        if i <= wordamt:
            words.append(line.strip('\n'))
        elif i > wordamt and len(tests) <= testamt:
            tests.append(line.strip('\n'))
        i += 1
    return (words, tests)


def run_tests(words, tests):
    results = []
    for t in tests:
        exp = re.compile(compile_re(t))
        r = [exp.search(w) and 1 or 0 for w in words]
        results.append(sum(r))
    return results


def compile_re(pattern):
    if not '(' in pattern:
        return pattern
    result = []
    in_parenthesis = False
    length = len(pattern)
    i = 0
    for l in list(pattern):
        if l == '(':
            in_parenthesis = True
        elif in_parenthesis and l == ')':
            in_parenthesis = False
        elif in_parenthesis and not length == i+1 and not pattern[i+1] == ')':
            l += '|'
        result.append(l)
        i += 1
    return ''.join(result)


def main():
    words, tests = parse_doc(open(LARGEINPUT, 'r'))
    result = run_tests(words, tests)
    case = 0
    for r in result:
        print('Case #{0}: {1}'.format(case, r))
        case += 1


if __name__ == "__main__":
    main()
