Example run in terminal:
  Prelude> :load RedBlackTree.hs
  Prelude> let myTree = foldr treeInsert Empty [1..12]
  Prelude> height myTree
  Prelude> display myTree

height : returns height of given tree
display : prints the given tree
