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

        result = cta.get_possible_knight_moves((2, 1))
        self.assertEquals(len(result), 3)

        cta.visited = []
        result = cta.get_possible_knight_moves((3, 2))
        self.assertEquals(len(result), 6)

    def test_search(self):
        cta = program.CaptureThemAll('a1', 'a2', 'b2')
        result = cta.search()
        self.assertEquals(result, 6)

        cta = program.CaptureThemAll('a1', 'b3', 'c5')
        result = cta.search()
        self.assertEquals(result, 2)

        cta = program.CaptureThemAll('b1', 'c3', 'a3')
        result = cta.search()
        self.assertEquals(result, 3)

        cta = program.CaptureThemAll('a5', 'b7', 'e4')
        result = cta.search()
        self.assertEquals(result, 3)

        cta = program.CaptureThemAll('h8', 'e2', 'd2')
        result = cta.search()
        self.assertEquals(result, 6)
