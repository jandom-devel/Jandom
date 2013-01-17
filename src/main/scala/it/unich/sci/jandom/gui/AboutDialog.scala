package it.unich.sci.jandom
package gui

import scala.swing._
  
private class CenteredLabel(s: String) extends Label(s) {
    xLayoutAlignment = 0.5
}

object AboutDialog extends Dialog {
  modal = true
  title = "About Jandom"
  contents = new BoxPanel(Orientation.Vertical) {
    contents += new CenteredLabel(name) {
      font = new Font(java.awt.Font.SERIF, 0, 36)
    }
    contents += Swing.VStrut(10)
    contents += new CenteredLabel("This is "+it.unich.sci.jandom.name+" "+version+", an experimental")
    contents += new CenteredLabel("static analyzer written in Scala")
    contents += Swing.VStrut(10)
    contents += new CenteredLabel ("\u00A9 2012-2013 Gianluca Amato")
    contents ++= Seq(Swing.VStrut(5), new Separator, Swing.VStrut(5))
    contents += new Button() {
      xLayoutAlignment = 0.5
      action = new Action("OK") {
    	def apply() { AboutDialog.visible = false }
      }
    }
    border = Swing.EmptyBorder(10)
  }
}
