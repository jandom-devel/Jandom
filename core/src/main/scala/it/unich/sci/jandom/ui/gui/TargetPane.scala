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

package it.unich.sci.jandom.ui.gui

import scala.swing.Component
import scala.swing.Action

/**
 * Each different target may implement a TargetPane to be supported by the UI.
 * @author Gianluca Amato
 *
 */
trait TargetPane extends Component {
  /**
   * When this method is called, the pane try to save the current program under analysis.
   * It returns `true` if saving succeeded (or was not necessary), `false` otherwise.
   */
  def ensureSaved(): Boolean

  /**
   * This method is call to uptoad the frame title
   */
  def updateFrameTitle()

  /**
   * This method performs the analysis of the current program and returns the result as a
   * string to be displayed in the output pane.
   */
  def analyze: Option[String]

  /**
   * This is the list of items to add to the File menu when this pane is active.
   */
  val fileMenuItems: Seq[Component]

  /**
   * This is the list of items to add to the Edit menu when this pane is active.
   */
  val editMenuItems: Seq[Component]

}