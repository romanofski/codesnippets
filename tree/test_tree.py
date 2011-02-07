import unittest
import tree


class TreeTest(unittest.TestCase):

    def test_tree(self):
        t = tree.BinaryTreeNode(3)
        t.insert(5)
        t.insert(4)
        self.assertEquals(t.left, None)
        self.assertEquals(t.right.value, 5)
        self.assertEquals(t.right.left.value, 4)

        t = tree.BinaryTreeNode(1)
        for i in [2,3,4,5,6]:
            t.insert(i)
        # linked list :(
        self.assertEquals(t.show_tree(), [1, [2, [3, [4, [5, [6]]]]]])

    def test_show_tree(self):
        t = tree.BinaryTreeNode(3)
        t.insert(5)
        t.insert(4)
        t.insert(2)
        self.assertEquals(t.show_tree(), [3, [2], [5, [4]]])

