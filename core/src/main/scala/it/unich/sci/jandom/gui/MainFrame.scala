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

package it.unich.sci.jandom
package gui

import domains._
import targets.slil.SLILStmt
import scala.swing._
import scala.swing.event._
import java.awt.GridBagConstraints
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import scala.swing.TabbedPane

class MainFrame extends Frame {

  val editorPane = new JandomEditorPane(this)
  val outputPane = new OutputPane
  val parametersPane = new ParametersPane
  val tabbedPane = new TabbedPane {
    pages += new TabbedPane.Page("Editor", new ScrollPane(editorPane))
    pages += new TabbedPane.Page("Output", new ScrollPane(outputPane))
    pages += new TabbedPane.Page("Parameters", parametersPane)
  }

  /**
   * This is the Action to invoke when user wants to quit the application
   */
  val quitAction = new Action("Quit") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK))
    def apply() {
      if (editorPane.ensureSaved) sys.exit(0)
    }
  }

  /**
   * This is the action to invoke when the user selects the About Box dialog
   */
  val aboutAction = new Action("About") {
    def apply() {
      AboutDialog.visible = true
    }
  }

  val analyzeAction = new Action("ANALYZE!") {
    def apply() {
      val source = editorPane.text
      val parser = parsers.RandomParser()
      parser.parseProgram(source) match {
        case parser.Success(program, _) =>
          val domain = parametersPane.selectedDomain
          val params = parametersPane.getParameters(program.asInstanceOf[SLILStmt])
          val ann = program.analyze(params)
          outputPane.text = outputPane.text + params.debugWriter.toString
          outputPane.text = outputPane.text + program.mkString(ann)
          tabbedPane.selection.index = 1
        case parser.NoSuccess(msg, next) =>
          Dialog.showMessage(editorPane, msg + " in line " + next.pos.line + " column " + next.pos.column,
            "Error in parsing source code", Dialog.Message.Error)
      }
    }
  }

  /**
   * Closing the frame causes the program to exit
   */
  override def closeOperation() {
    quitAction()
  }
  init()

  def init() {
    title = "Jandom"

    contents = new BorderPanel {
      val analyzeButton = new Button(analyzeAction)
      layout(tabbedPane) = BorderPanel.Position.Center
      layout(analyzeButton) = BorderPanel.Position.South
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(editorPane.newAction)
        contents += new MenuItem(editorPane.openAction)
        contents += new Separator
        contents += new MenuItem(editorPane.saveAction)
        contents += new MenuItem(editorPane.saveAsAction)
        contents += new Separator
        contents += new MenuItem(quitAction)
      }
      contents += new Menu("Edit") {
        contents += new MenuItem(editorPane.undoAction)
        contents += new MenuItem(editorPane.redoAction)
        contents += new Separator
        contents += new MenuItem(editorPane.cutAction)
        contents += new MenuItem(editorPane.copyAction)
        contents += new MenuItem(editorPane.pasteAction)
      }
      contents += new Menu("Tool") {
        contents += new MenuItem(outputPane.clear)
      }
      contents += new Menu("Help") {
        contents += new MenuItem(aboutAction)
      }
    }

    bounds = new Rectangle(100, 100, 800, 600)

    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  }
}
