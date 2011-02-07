

class BinaryTreeNode(object):
    """binary tree."""

    left = None
    right = None
    __parent__ = None

    def __init__(self, value):
        self.value = value

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
        children = []
        if node.left is not None:
            children.append(node.left)
        if node.right is not None:
            children.append(node.right)
        for child in children:
            sub = [child.value]
            sub += self.show_tree(child)
            result.append(sub)
        return result
