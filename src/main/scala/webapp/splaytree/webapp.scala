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

    def getNumberElement(node: SplayNode): TypedTag[Div] = {
      val data = node.value
      div(cls:="letter-node", s"$data")
    }

    def getAllNumbers(node: SplayNode, formattingCB: (SplayNode) => TypedTag[Div], nodeType: String = ""): TypedTag[Div] = {
      div(
        cls:=s"tree-box $nodeType",
        node.left.map(getAllNumbers(_, formattingCB, "child")).getOrElse(div()),
        formattingCB(node),
        node.right.map(getAllNumbers(_, formattingCB, "child")).getOrElse(div())
      )
    }

    val inputBox: InputBox = new InputBox("enter a number", "Add a Node to the Splay Tree")

    var st = new SplayTree((x: scala.Option[SplayNode]) => {

      x.map(node => {
        inputBox.updateVis(
          getAllNumbers(node, getNumberElement)
        )
        node
      })

    })

    inputBox.setRenderCallBack((number: Int) => {
      st.addNode(number)
      st.find(number)
    })

  }
}
