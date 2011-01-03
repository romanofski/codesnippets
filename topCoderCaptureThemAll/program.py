import itertools


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

    def search(self, pos=None, moves=None, length=0):
        open = []
        if pos is None and moves is None:
            moves = [[self.knight]]

        while moves:
            children = moves[0]
            del moves[0]
            open = [self.get_possible_knight_moves(x) for x in children]
            if self.queen in children:
                self.queen_found = True
                self.visited = []
                open = [self.get_possible_knight_moves(self.queen)]
                break
            if self.rook in children:
                self.rook_found = True
                self.visited = []
                open = [self.get_possible_knight_moves(self.rook)]
                break

        if sum((self.queen_found, self.rook_found)) == 2:
            return length
        length += 1
        return self.search(moves=open, length=length)

    def get_possible_knight_moves(self, pos):
        self.visited.append(pos)
        moves = []
        for x, y in itertools.permutations([2, 1, -1, -2], 2):
            if sum((x,y)) == 0:
                continue

            newpos = (pos[0] + x, pos[1] + y)

            if not self.is_out_of_bounds(newpos) and not newpos in self.visited:
                moves.append(newpos)

        return moves

    def is_out_of_bounds(self, pos):
        return min(pos) < 1 or max(pos) >= self.board[0]

