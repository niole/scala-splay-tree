package splaytree

import scala.scalajs.js.JSApp
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import dom._
import dom.html._
import window.setTimeout

object SplayTreeVis extends JSApp {
  def main(): Unit = {

    //map over the red black tree and return stuff
    //go to leaves, and position everything from there
    //mvp of the mvp: a string

    var st = new SplayTree()

    val inputBox: InputBox = new InputBox("enter a number", "Add a Node to the Splay Tree")

    inputBox.setRenderCallBack((number: Int) => {
//      val tm = setTimeout(() => inputBox.updateVis(ns), 500)
    })

    def getNumberElement(node: SplayNode): TypedTag[Div] = {
      val data = node.value
      div(cls:="letter-node", s"$data")
    }

    def getAllNumbers(node: SplayNode, formattingCB: (SplayNode) => TypedTag[Div], nodeType: String): TypedTag[Div] = {
      div(
        cls:=s"tree-box $nodeType",
        node.left.map(getAllNumbers(_, formattingCB, "child")).getOrElse(div()),
        formattingCB(node),
        node.right.map(getAllNumbers(_, formattingCB, "child")).getOrElse(div())
      )
    }


  }
}
