/**
 * Copyright 2013 Gianluca Amato
 * 
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.ui.gui

import it.unich.jandom._
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
    contents += new CenteredLabel("This is "+softwareName+" "+version+", an experimental")
    contents += new CenteredLabel("static analyzer written in Scala")
    contents += Swing.VStrut(10)
    contents += new CenteredLabel ("\u00A9 2012-2013 Gianluca Amato")
    contents ++= Seq(Swing.VStrut(5), new Separator, Swing.VStrut(5))
    contents += new Button() {
      xLayoutAlignment = 0.5
      action = new Action("OK") {
    	def apply() = { AboutDialog.visible = false }
      }
    }
    border = Swing.EmptyBorder(10)
  }
}
