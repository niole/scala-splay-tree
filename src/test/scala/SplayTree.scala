package splaytree

import org.scalatest.FlatSpec

class SplayTreeTest extends FlatSpec {

   "A SplayTree" should "initialize to an empty tree" in {
    val st = new SplayTree()

    assert(st.getTree === None)
  }

   it should "maintain the BST ordering property" in {
    val st = new SplayTree()

    st.addNode(5)
    st.addNode(3)
    st.addNode(13)
    st.addNode(9)

    val root = st.getTree

    assert(root.get !== None)
    assert(root.get.value === 5)
    assert(root.get.left.get.value === 3)
    assert(root.get.right.get.value === 13)
    assert(root.get.right.get.left.get.value === 9)
  }

}
