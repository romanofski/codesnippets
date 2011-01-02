import unittest
import program


class CaptureThemAllTest(unittest.TestCase):

    def test_get_coordinates(self):
        cta = program.CaptureThemAll('a1', 'b3', 'c5')
        result = cta.get_coordinates('a1')
        self.assertEquals(result, (1, 1))

        self.assertEquals(cta.get_coordinates('b3'), (2, 3))
        self.assertEquals(cta.get_coordinates('c5'), (3, 5))
