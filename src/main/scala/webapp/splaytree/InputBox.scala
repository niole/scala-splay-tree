package splaytree

import org.scalajs.dom
import dom._
import org.scalajs.dom.html._

import scalatags.JsDom.all._
import scalatags.JsDom.short.*
import window.{ setTimeout, clearTimeout }

class InputBox(placeholder: String, header: String, delay: Int = 200) {
  val i: Input = input(*.placeholder:=placeholder).render
  val visContainer: Div = div.render
  val container: Div = div.render
  var renderCallBack: Int => Unit  = (n: Int) => {}
  val debouncer: () => Unit = getDebouncer(onKeyUp)

  document.body.appendChild(
    container.appendChild(
      div(
        h1(header),
        div(i),
        visContainer
      ).render
    )
  )

  def onKeyUp(): Unit = {
    val n = i.value
    renderCallBack(n.toInt)
    i.value = ""
  }

  def getDebouncer(F: () => Unit): () => Unit = {
    var timeout = 0

    def debounce(): Unit = {
      clearTimeout(timeout)
      timeout = setTimeout(() => F(), delay)
    }

    debounce
  }

  //could be another event
  i.onkeyup = (e: dom.Event) => {
    debouncer()
  }

  def setRenderCallBack(cb: Int => Unit): Unit = renderCallBack = cb

  def updateVis(newVisData: Div): Unit = {
    visContainer.innerHTML = ""
    visContainer.appendChild(div(newVisData).render)
  }

}
