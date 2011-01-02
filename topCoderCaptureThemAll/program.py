

class CaptureThemAll(object):

    board = (8,8)

    def __init__(self, knight, queen, rook):
        self.knight = knight
        self.queen = queen
        self.rook = rook

    def get_coordinates(self, strpos):
        pos = tuple(strpos)
        rows = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
        return (rows.index(pos[0]) + 1, int(pos[1]))
