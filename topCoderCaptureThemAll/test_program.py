import unittest
import program


class CaptureThemAllTest(unittest.TestCase):

    def test_get_coordinates(self):
        cta = program.CaptureThemAll('a1', 'b3', 'c5')
        result = cta.get_coordinates('a1')
        self.assertEquals(result, (1, 1))

        self.assertEquals(cta.get_coordinates('b3'), (2, 3))
        self.assertEquals(cta.get_coordinates('c5'), (3, 5))
        self.assertEquals(cta.get_coordinates('h8'), (8, 8))
        self.assertEquals(cta.knight, (1, 1))

    def test_get_possible_knight_moves(self):
        cta = program.CaptureThemAll('a1', 'b3', 'c5')
        result = cta.get_possible_knight_moves((1, 1))
        self.assertEquals(len(result), 2)
        self.assertEquals(sorted(result), [(2, 3), (3, 2)])

        result = cta.get_possible_knight_moves((4, 4))
        self.assertEquals(len(result), 8)

    def test_search(self):
        cta = program.CaptureThemAll('a1', 'b3', 'c5')
        result = cta.search()
        self.assertEquals(result, 2)

        cta = program.CaptureThemAll('b1', 'c3', 'a3')
        result = cta.search()
        self.assertEquals(result, 3)

