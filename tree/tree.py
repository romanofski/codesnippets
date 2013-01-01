

class BinaryTreeNode(object):
    """binary tree."""

    __parent__ = None

    def __init__(self, value, left=None, right=None, parent=None):
        self.value = value
        self.left = left
        self.right = right
        self.__parent__ = parent

    def __repr__(self):
        return '<{0} - {1}>'.format(self.__class__.__name__, self.value)

    def __str__(self):
        direction = 'r'
        if self.__parent__:
            node = self.__parent__
            direction = node.left.value == self.value and 'l' or 'r'
        return '{0}:{1}'.format(direction, self.value)

    def get_children(self):
        children = [self.left, self.right]
        return [x for x in children if x is not None]

    def has_children(self):
        return bool(self.get_children())

    def insert(self, value, node=None):
        if node is None:
            node = self

        bt = BinaryTreeNode(value)
        bt.__parent__ = node

        if node.left is None and node.value >= value:
            node.left = bt
            return BinaryTreeNode(node.value, left=node.left)
        if node.right is None and node.value <= value:
            node.right = bt
            return BinaryTreeNode(node.value, right=node.right)
        if node.right.value <= value:
            return BinaryTreeNode(
                node.right.value, node.right.left, node.right.right,
                node.right).insert(value)
        else:
            return BinaryTreeNode(
                node.left.value, node.left.left, node.left.right,
                node.left.__parent__).insert(value)


    def delete(self, value, node=None):
        children = self.get_children()
        if node is None:
            children.insert(0, self)
        for child in self.get_children():
            if child.value == value and not child.has_children():
                child.__parent__.left = None
                return
            child.delete(value)

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
