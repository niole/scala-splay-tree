/**
 * zig and zag are just large scale tree rotations that result in the element
 * searched for ending up at the root of the tree
 **/

 object Main extends App {
   val st = new SplayTree()
   println(st)
 }

 class SplayTree {
   var tree: Option[SplayNode] = None

   private[this] def cmp(
     root: Option[SplayNode], otherValue: Int, comparator: (Int, Int) => Boolean
   ): Boolean = root.map(r => comparator(r.value, otherValue)).getOrElse(false)

   private[this] def init(value: Int): Option[SplayNode] = Some(new SplayNode(value, None, None))

   private[this] def add(newValue: Int): Option[SplayNode] = tree.map(root => root.add(newValue)).orElse(init(newValue))

   private[this] def splay(targetValue: Int, root: Option[SplayNode]): Option[SplayNode] = {
     //only run if tree contains target and target is not root

     //always rotate the root the right way
     //then as you transfer the leaves
     //make sure to do an opposite rotation in need to

     //if rotating root right, and target is greater than root's left child
     //accomplish a left rotation for this subnode and then continue main rotation


     if (cmp(root, targetValue, (x, y) => x == y)) root //done
     else if (cmp(root, targetValue, (x, y) => x > y)) {

       val newRoot = root.flatMap(
         _.left.flatMap(l => {
           if (targetValue > l.value) {
             //rotate right,
             //left becomes parent's right, parent becomes left child

             l.right.map(r => {
                 val newLeft = Some(new SplayNode(l.value, l.left, r.left))
                 new SplayNode(r.value, newLeft, r.right)
               })
           }
           else Some(l)
         })
       )

       //rotate root
       //pass newRoot's right to old root's left, old root becomes newRoot's right
       splay(
         targetValue,
         newRoot.map(nr =>
           new SplayNode(
             nr.value,
             nr.left,
             root.map(r => new SplayNode(r.value, nr.right, r.right))
           )
         )
       )
     }
     else if (cmp(root, targetValue, (x, y) => x < y)) {
       //rotate left, root less than target
       val newRoot = root.flatMap(
         _.right.flatMap(r => {
           if (targetValue < r.value) {

             r.left.map(l => {
                 new SplayNode(
                   l.value,
                   l.left,
                   Some(
                     new SplayNode(
                       r.value,
                       l.right,
                       r.right
                     )
                  )
               )
             })

          }
          else Some(r)
       })
      )

       //rotate left and call splay
       //make root left child et c
       splay(
         targetValue,
         newRoot.map(nr =>
           new SplayNode(
             nr.value,
             root.map(r => new SplayNode(r.value, r.left, nr.left)),
             nr.right
           )
         )
       )

     }
     else root //you messed up somewhere

   }

   def addNode(newValue: Int): Unit = tree = add(newValue)

   def find(newValue: Int): Option[SplayNode] = {
     val found = tree.map(_.find(newValue))
     found.flatMap(_ => splay(newValue, tree))
   }

 }

 class SplayNode(v: Int, l: Option[SplayNode], r: Option[SplayNode]) {
   val value: Int = v
   val left: Option[SplayNode] = l
   val right: Option[SplayNode] = r

   def add(newValue: Int): SplayNode = {
     if (newValue < value) {

        new SplayNode(
          value,
          left.map(_.add(newValue)).orElse(Some(new SplayNode(newValue, None, None))),
          right
        )

     }
     else {
       new SplayNode(
         value,
         left,
         right.map(_.add(newValue)).orElse(Some(new SplayNode(newValue, None, None)))
       )
     }
  }

  /**
   * once find correct node, zig and zag it back up to the root
   * and return the root
   **/
  def find(withValue: Int): Option[SplayNode] = {
    if (withValue < value) {
      left.map(_.find(withValue)).getOrElse(None)
    }
    else if (withValue > value) {
      right.map(_.find(withValue)).getOrElse(None)
    }
    else if (withValue == value) Some(this) //found it
    else None
  }

 }
