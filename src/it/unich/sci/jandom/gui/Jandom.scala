/**
 * The main Jandom GUI
 *
 * Copyright 2011 Gianluca Amato
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

package it.unich.sci.jandom.gui

import scala.swing._
import scala.swing.event._

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
object Jandom extends SimpleSwingApplication {
  def top = new MainFrame {
    var nClicks = 0
    
    title = "Jandom GUI"
    val button = new Button  {
      text = "Click me"
    }
    val label = new Label {
      text = "No button clicks registered" 
    }
    listenTo(button)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += button
      contents += label
      border = Swing.EmptyBorder(30,30,10,30)
    }
    reactions += {
      case ButtonClicked(b) => 
        nClicks +=1
        label.text = "Number of button clicks: "+nClicks        
    }
  }

}