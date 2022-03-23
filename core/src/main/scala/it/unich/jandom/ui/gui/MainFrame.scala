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

import java.awt.event.{ InputEvent, KeyEvent }

import scala.swing._

import javax.swing.KeyStroke
import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE

class MainFrame extends Frame {


  object Mode extends Enumeration {
    type Mode = Value
    val Random, Asm, Soot, Fast = Value
    val default = Random
  }

  import Mode._

  private var realMode: Mode = Mode.default
  private var currentEditorPane: TargetPane = modeToPane(realMode)

  val tabbedPane = new TabbedPane {
    pages += new TabbedPane.Page("Editor", currentEditorPane)
    pages += new TabbedPane.Page("Output", new ScrollPane(outputPane))
    pages += new TabbedPane.Page("Parameters", parametersPane)
  }

  /**
   * The panes which compose the GUI
   */
  lazy val jandomEditorPane = new JandomEditorPane(this)
  lazy val asmEditorPane = new ASMEditorPane(this)
  lazy val sootEditorPane = new SootEditorPane(this)
  lazy val fastEditorPane = new FastEditorPane(this)
  lazy val outputPane = new OutputPane
  lazy val parametersPane = new ParametersPane


  /**
   * Map a mode to a corresponding editor pane
   */
  def modeToPane(m: Mode) = m match {
    case Random => jandomEditorPane
    case Soot => sootEditorPane
    case Asm  => asmEditorPane
    case Fast  => fastEditorPane
  }

  /**
   * This is the Action to invoke when user wants to quit the application
   */
  val quitAction = new Action("Quit") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_DOWN_MASK))
    def apply() = {
      if (currentEditorPane.ensureSaved()) sys.exit(0)
    }
  }

  /**
   * This is the action to invoke when the user selects the About Box dialog
   */
  val aboutAction = new Action("About") {
    def apply() = {
      AboutDialog.visible = true
    }
  }

  /**
   * This is the action to invoke when the user press the Analyze button
   */
  val analyzeAction = new Action("ANALYZE!") {
    def apply() = {
      currentEditorPane.analyze match {
        case Some(output) =>
          outputPane.text = outputPane.text + output
          tabbedPane.selection.index = 1
        case None =>
      }
    }
  }

  val randomAction: Action = new Action("Random") {
    toolTip = "Analysis of C-style programs"
    def apply() = {
      mode = Random
    }
  }

  val asmAction: Action = new Action("ASM") {
    toolTip = "Analysis of Java bytecode thorugh the ASM library"
    def apply() = {
      mode = Asm
    }
  }

  val sootAction: Action = new Action("Soot") {
    toolTip = "Analysis of Java bytecode thorugh the Soot library"
    def apply() = {
      mode = Soot
    }
  }

  val fastAction: Action = new Action("Fast") {
    toolTip = "Analysis of  Alice library"
    def apply() = {
      mode = Fast
    }
  }
  init()


  def init(): Unit = {
    title = "Jandom"

    contents = new BorderPanel {
      val analyzeButton = new Button(analyzeAction)
      layout(tabbedPane) = BorderPanel.Position.Center
      layout(analyzeButton) = BorderPanel.Position.South
    }
    setMenuBar()
    bounds = new Rectangle(100, 100, 800, 600)
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    mode = Mode.Random
  }

  def setFileMenu(): Unit = {
    val fileMenu = menuBar.menus(0)
    fileMenu.contents.clear()
    fileMenu.contents ++= currentEditorPane.fileMenuItems
    if (!currentEditorPane.fileMenuItems.isEmpty)
      fileMenu.contents += new Separator
    fileMenu.contents += new MenuItem(quitAction)
  }

  def setEditMenu(): Unit = {
    val editMenu = menuBar.menus(1)
    editMenu.contents.clear()
    if (currentEditorPane.editMenuItems.isEmpty)
      editMenu.enabled = false
    else
      editMenu.enabled = true
    editMenu.contents ++= currentEditorPane.editMenuItems
  }

  def setMenuBar(): Unit = {
    val randomMode = new RadioMenuItem("")
    randomMode.action = randomAction
    val asmMode = new RadioMenuItem("")
    asmMode.action = asmAction
    val sootMode = new RadioMenuItem("")
    sootMode.action = sootAction
    val fastMode = new RadioMenuItem("")
    fastMode.action = fastAction

    val buttonGroups = new ButtonGroup(randomMode, asmMode, sootMode, fastMode)
    menuBar = new MenuBar {
      contents += new Menu("File")
      contents += new Menu("Edit")
      contents += new Menu("Tool") {
        contents += new MenuItem(outputPane.clear)
        contents += new Separator
        contents ++= buttonGroups.buttons
      }
      contents += new Menu("Help") {
        contents += new MenuItem(aboutAction)
      }
    }

    buttonGroups.select(realMode match {
      case Random => randomMode
      case Asm => asmMode
      case Soot => sootMode
      case Fast => fastMode
    })
  }

  /**
   * Closing the frame causes the program to exit
   */
  override def closeOperation() = {
    quitAction()
  }

  def mode = realMode

  def mode_=(m: Mode): Unit = {
    realMode = m
    currentEditorPane = m match {
      case Asm => asmEditorPane
      case Soot => sootEditorPane
      case Random => jandomEditorPane
      case Fast => fastEditorPane
    }
    tabbedPane.pages(0).content = currentEditorPane
    // repaint is needed to avoid corruption in display
    tabbedPane.repaint()
    setFileMenu()
    setEditMenu()
    currentEditorPane.select()
  }
}
