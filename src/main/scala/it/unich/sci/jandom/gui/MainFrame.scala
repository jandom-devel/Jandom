package it.unich.sci.jandom
package gui

import domains._
import scala.swing._
import scala.swing.event._
import java.awt.GridBagConstraints
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import java.awt.event.InputEvent

class MainFrame extends Frame {

  val domainList = Seq(domains.BoxDouble, domains.PPLBoxDouble, domains.PPLCPolyhedron)
  val editorPane = new JandomEditorPane
  val quitAction = new Action("Quit") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK))
    def apply() { close() }
  }
  val domainComboBox = new ComboBox(domainList)
  
  init()

  override def closeOperation() {
    sys.exit(0)
  }

  def init() {
    title = "Jandom"

    contents = new BorderPanel {
      val tabbedPane = new TabbedPane {
        val outputPane = new EditorPane
        val parameterPanel = new GridBagPanel {
          border = Swing.EmptyBorder(5, 5, 5, 5)
          layout(new Label("Domain:")) = new Constraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER,
            GridBagConstraints.NONE, new Insets(0, 0, 5, 5), 0, 0)          
          layout(domainComboBox) = new Constraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER,
            GridBagConstraints.HORIZONTAL, new Insets(0, 0, 5, 0), 0, 0)
        }
        pages += new TabbedPane.Page("Editor", editorPane)
        pages += new TabbedPane.Page("Output", outputPane)
        pages += new TabbedPane.Page("Parameters", parameterPanel)
      }
      val analyzeButton = new Button()
      analyzeButton.action = new Action("ANALYZE!") {
        def apply() {
          val source = editorPane.text
          val parsed = parsers.RandomParser().parseProgram(source)
          if (parsed.successful) {
            val program = parsed.get
            val domain = domains.BoxDouble
            val params = new targets.Parameters[domains.BoxDouble, targets.slil.SLILStmt](domain, program)
            val ann = program.analyze(params)
            tabbedPane.outputPane.text = program.mkString(ann)
            tabbedPane.selection.index = 1
          }
        }
      }
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
    }
    bounds = new Rectangle(100, 100, 800, 600)
  }
}
