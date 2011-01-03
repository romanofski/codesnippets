

class CaptureThemAll(object):

    board = (9,9)
    searchmax = 10
    queen_found = False
    rook_found = False

    def __init__(self, knight, queen, rook):
        self.knight = self.get_coordinates(knight)
        self.queen = self.get_coordinates(queen)
        self.rook = self.get_coordinates(rook)
        self.visited = []

    def get_coordinates(self, strpos):
        pos = tuple(strpos)
        rows = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
        return (rows.index(pos[0]) + 1, int(pos[1]))

    def search(self, pos=None, moves=None, length=1):
        if pos is None:
            pos = self.knight
        if moves is None:
            moves = self.get_possible_knight_moves(pos)

        for m in moves:
            if m == self.queen:
                self.queen_found = True
            if m == self.rook:
                self.rook_found = True

        while moves:
            if (True, True) == (self.queen_found, self.rook_found):
                break

            newpos = moves[0]
            del moves[0]
            moves = self.get_possible_knight_moves(newpos)

            length += 1
            self.search(newpos, moves, length)
        return length

    def get_possible_knight_moves(self, pos):
        self.visited.append(pos)
        moves = []
        for x, y in [(2, 1), (2, -1), (-2, 1), (-2, -1)]:
            newpos = (pos[0] + x, pos[1] + y)
            if newpos in self.visited:
                continue

            if not self.is_out_of_bounds(newpos):
                moves.append(newpos)
            newpos = list(newpos)
            newpos.reverse()
            if not self.is_out_of_bounds(newpos):
                moves.append(tuple(newpos))

        return moves

    def is_out_of_bounds(self, pos):
        return min(pos) <= 1 or max(pos) >= self.board[0]

