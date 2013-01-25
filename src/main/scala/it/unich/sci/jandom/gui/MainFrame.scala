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

class MainFrame extends Frame {

  val editorPane = new JandomEditorPane(this)

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
      val statusBar = new Label("dd")
      val tabbedPane = new TabbedPane {
        val outputPane = new EditorPane
        val parametersPane = new ParametersPane
        pages += new TabbedPane.Page("Editor", new ScrollPane(editorPane))
        pages += new TabbedPane.Page("Output", new ScrollPane(outputPane))
        pages += new TabbedPane.Page("Parameters", parametersPane)
      }
      val analyzeButton = new Button()
      analyzeButton.action = new Action("ANALYZE!") {
        def apply() {
          val source = editorPane.text
          val parser = parsers.RandomParser()
          parser.parseProgram(source) match {
            case parser.Success(program, _) =>
              val domain = tabbedPane.parametersPane.selectedDomain
              val params = tabbedPane.parametersPane.getParameters(program.asInstanceOf[SLILStmt])
              val ann = program.analyze(params)
              tabbedPane.outputPane.text = program.mkString(ann)
              tabbedPane.selection.index = 1
            case parser.NoSuccess(msg, next) =>
              statusBar.text = msg + " in line " + next.pos.line + " column " + next.pos.column
          }
        }
      }
      val southPanel = new BorderPanel {
        layout(analyzeButton) = BorderPanel.Position.North
        layout(statusBar) = BorderPanel.Position.South
      }
      layout(tabbedPane) = BorderPanel.Position.Center
      layout(southPanel) = BorderPanel.Position.South
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
      contents += new Menu("Help") {
        contents += new MenuItem(aboutAction)
      }
    }

    bounds = new Rectangle(100, 100, 800, 600)

    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  }
}
