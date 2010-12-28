import unittest
import io
import codejam2009a


class Codejan2009Test(unittest.TestCase):

    def test_parsedoc(self):
        input = io.StringIO('2 2 2\nab\nbc\na(c)\n(bc)a')
        words, tests = codejam2009a.parse_doc(input)
        self.assertEqual(len(words), 2)
        self.assertEqual(len(tests), 2)
        self.assertEqual(['ab', 'bc'], sorted(words))
