

class BinaryTreeNode(object):
    """binary tree."""

    left = None
    right = None
    __parent__ = None

    def __init__(self, value, left=None, right=None, parent=None):
        self.value = value
        self.left = left
        self.right = right
        self.__parent__ = parent

    def get_children(self):
        children = [self.left, self.right]
        return [x for x in children if x is not None]

    def insert(self, value, node=None):
        if node is None:
            node = self

        if node.left is None and node.value >= value:
            node.left = BinaryTreeNode(value)
            node.left.__parent__ = node
            return
        if node.right is None and node.value <= value:
            node.right = BinaryTreeNode(value)
            node.right.__parent__ = node
            return
        if value >= node.value:
            self.insert(value, node.right)
        if value <= node.value:
            self.insert(value, node.left)

    def delete(self, value, node=None):
        if node is None:
            node = self
        if node.left is not None and node.left.value == value:
            node.left = None
        if node.right is not None and node.right.value == value:
            node.right = None
        for child in self.get_children():
            child.delete(value)

    def __str__(self):
        direction = 'r'
        if self.__parent__:
            node = self.__parent__
            direction = node.left.value == self.value and 'l' or 'r'
        return '{0}:{1}'.format(direction, self.value)

    def show_tree(self, node=None):
        """ Returns a list of values by left to right depth first
            traversal.
        """
        result = []
        if node is None:
            node = self
            result.append(node.value)
        for child in self.get_children():
            result.append(child.show_tree())
        return result
