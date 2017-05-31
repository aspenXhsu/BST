package com.xactly.sample


class binarySearchTreeNode(thisValue: Int, thisLeft: binarySearchTreeNode, thisRight: binarySearchTreeNode, thisName: String) {
      var value = thisValue
      var right = thisRight
      var left = thisLeft
      var name = thisName

  // This class does not have a root, instead, I just pass the self into every function (logical error)
     
  
      
      
  def exists(wantedValue: Int, root:binarySearchTreeNode): Boolean = {
    var current: binarySearchTreeNode = root
    while (current != null) {
      if (current.value == wantedValue){
        return true
      }
        else if (current.value < wantedValue){
        current = current.right
      } else {
        current = current.left
      }
      
    }
    return false
  }
      
      
   def add(root:binarySearchTreeNode, key:Int): binarySearchTreeNode =  {
     var current: binarySearchTreeNode = root
     if (current != null) {
       current = addHelper(current, key)
       } else {
         current = new binarySearchTreeNode(key, null, null, null)
       }
       return current;
   }
   
   def addHelper(root: binarySearchTreeNode, key:Int): binarySearchTreeNode = {
     var current: binarySearchTreeNode = root
     if (current == null) {
       current = new binarySearchTreeNode(key,null,null,null)
       return current;
     }
     val result = (key: Int) compareTo (current.value: Int)
     if (result < 0) {
       current.left = addHelper(current.left, key)
     } else if (result > 0) {
       current.right = addHelper(current.right, key)
     }
     return current;
   }
   
   
   def remove(root: binarySearchTreeNode, key: Int): binarySearchTreeNode = {
     var current: binarySearchTreeNode = root
     if (current == null) {
       return current
     } 
     if (key < current.value) {
       current.left = remove(current.left, key)
     } else if (key > current.value) {
       current.right = remove(current.right, key)
     } else {
       if (current.left == null) {
         return current.right
     } else if (current.right == null) {
       return current.left
       } else {
         current.value = minValue(current.right)
         current.right = remove(current.right, current.value)
       }
     }
     return current
   }
   
   
   
   def minValue(root: binarySearchTreeNode): Int = {
     var current: binarySearchTreeNode = root
     var smallest: Int = current.value
     while (current.left != null){
       smallest = current.left.value
       current = current.left
     }
     return smallest
   }
      
      
      def inOrder(root:binarySearchTreeNode): Unit = {
        if (root == null){
          return  
        }
        else {
        inOrder(root.left)
        println(root.name)
        print(":"+ root.value.toString)
        println(" ")
        inOrder(root.right)
        }
         
      }
      
      
      def aveNumLayers(root:binarySearchTreeNode): Int = {
        var starting: Int = 0
        var current: binarySearchTreeNode = root
        while (current != null){
          starting += 1
          current = current.left
        }
         return starting 
      }
         
      
      
      
      def printUglyTree(thisArray: Array[Int],root: binarySearchTreeNode, numLayers: Int) = {
        var placeHolder: Int = 0
        var placeHolder2: Int = 0
        var k = Array(2,3,5,5,6,7,8,9,10)
        var s = Array(1,2,4,8,16,32,64)
        val r: Int = numLayers*7
        while (placeHolder < numLayers) {
          var stringy: String = " "*(r/(k(placeHolder)))
          var yay: Int = s(placeHolder)
          var myBoo: Boolean = false
          var helpMe: String = ""
          while (yay > 0) {
            helpMe = helpMe + stringy + thisArray(placeHolder2).toString
            yay -= 1
            placeHolder2 += 1            
            }
          println(helpMe)
          placeHolder += 1
          
        }
      }
       
      
      
      

      
  
 
  
}

object Demo2 {
   def main(args: Array[String]) {
      val leftBranch = new binarySearchTreeNode(50, 
          new binarySearchTreeNode(30,null,null,"Jeremey"), 
          new binarySearchTreeNode(60, null,null, "kandarp"), 
          "Hiren")
      
      val rightBranch = new binarySearchTreeNode(100, 
          new binarySearchTreeNode(80,null,null,"Gowri"), 
          new binarySearchTreeNode(90, null,null, "Ron"), 
          "Ved")
     
      val thisTree = new binarySearchTreeNode(70,leftBranch,rightBranch,"aspen")
      //print(thisTree.exists(67,thisTree))
    
      val breadthFirst = Array(70,50,100,30,60,80,90)
      thisTree.printUglyTree(breadthFirst, thisTree, 3)
      thisTree.inOrder(thisTree)
     
      //thisTree.add(thisTree, 45)
      //thisTree.inOrder(thisTree)
      //thisTree.remove(thisTree, 45)
      //thisTree.inOrder(thisTree)
   }
}

  




