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

    def test_run_tests(self):
        results = codejam2009a.run_tests(['ab', 'bc'], ['(ab)c', '(bc)a'])
        self.assertEquals(len(results), 2)
        self.assertEquals(1, results[0])
        self.assertEquals(0, results[1])

        results = codejam2009a.run_tests(['rzqnjqwslucxlxb'],
                                         ['xvrmlxabgqivqzc',
                                          'rzqnj(cmqs)ws(lmd)uc(xyrs)lxb'])
        self.assertEquals([0, 1], results)

        results = codejam2009a.run_tests(['cfzqhueklzpptwc',
                                          'dfzuhuenlzpuckc',
                                          'cvograiddvhrrds',],
                                         ['(cdn)(efs)(zkr)(pqu)(fhs)(cu)(wev)(kn)(klc)(lzg)(lpx)(puz)(ctv)(wkl)(ucd)'])
        self.assertEquals([2], results)

    def test_compile_re(self):
        result = codejam2009a.compile_re('(ab)c')
        self.assertEquals(result, '(a|b)c')
