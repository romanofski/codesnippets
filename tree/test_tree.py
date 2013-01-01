import unittest
import tree


class BinaryTreeTest(unittest.TestCase):

    def test_insert(self):
        t = tree.BinaryTreeNode(3)
        t = t.insert(5)
        import pdb; pdb.set_trace() 
        t = t.insert(4)
        self.assertEquals(t.left, None)
        self.assertEquals(t.right.value, 5)
        self.assertEquals(t.right.left.value, 4)

        t = tree.BinaryTreeNode(1)
        for i in [2,3,4,5,6]:
            t.insert(i)
        # linked list :(
        self.assertEquals(t.show_tree(), [1, [2, [3, [4, [5, [6]]]]]])

    def test_delete(self):
        t = tree.BinaryTreeNode(5)
        t.insert(2)
        t.insert(4)
        t.insert(10)
        t.insert(15)
        t.insert(11)

        self.assertEquals(t.show_tree(), [5, [2, [4]], [10, [15, [11]]]])
        t.delete(11)
        self.assertEquals(t.show_tree(), [5, [2, [4]], [10, [15]]])
        t.delete(10)
        self.assertEquals(t.show_tree(), [5, [2, [4]], [15]])

    def test_show_tree(self):
        t = tree.BinaryTreeNode(3)
        t.insert(5)
        t.insert(4)
        t.insert(2)
        self.assertEquals(t.show_tree(), [3, [2], [5, [4]]])

