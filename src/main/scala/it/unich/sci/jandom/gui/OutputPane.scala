package it.unich.sci.jandom.gui

import scala.swing._

class OutputPane extends EditorPane {
    val clear = new Action("Clear Output") {
		def apply { OutputPane.this.text = "" }
    }
}