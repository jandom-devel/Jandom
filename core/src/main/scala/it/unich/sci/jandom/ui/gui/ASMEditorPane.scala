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

import java.awt.event.{ InputEvent, KeyEvent }
import java.io.{ File, FileInputStream }

import scala.swing.{Action, BorderPanel, BoxPanel, ComboBox, EditorPane, FileChooser, Label, MenuItem, Orientation, ScrollPane}
import scala.swing.Dialog

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ ClassNode, MethodNode }

import it.unich.sci.jandom._
import it.unich.sci.jandom.targets.jvm._
import it.unich.sci.jandom.targets.Parameters

import javax.swing.KeyStroke

/**
 * This is the pane used to select the class and method to analyzer for
 * the ASM target.
 * @author Gianluca Amato
 *
 */
class ASMEditorPane(val frame: MainFrame) extends BorderPanel with TargetPane {

  /*
   * The EditorPane which is part of the JVMEditorPane
   */
  private val editorPane = new EditorPane
  private val fileChooser = new FileChooser(new File("."))
  private var methodComboBox: ComboBox[String] = new ComboBox(Seq())
  private val methodSelector = new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Method: ") { tooltip = "Choose the method to analyze." }
    contents += methodComboBox
  }
  /*
   * The list of currently available methods 
   */
  private var methodList = Seq[MethodNode]()

  /*
   * The method currently selected, or None if no method is selected.
   */
  private var method: Option[Method] = None

  editorPane.editable = false
  layout(new ScrollPane(editorPane)) = BorderPanel.Position.Center
  layout(methodSelector) = BorderPanel.Position.North

  val methodSelectAction = new Action("Method Select") {
    def apply() {
      val methodName = methodComboBox.selection.item
      method = Some(new Method(methodList.find(_.name == methodName).get))
      editorPane.text = method.get.toString
    }
  }
  val newAction = new Action("New") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK))
    def apply() { editorPane.text = "" }
  }

  val openAction = new Action("Open...") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK))
    def apply() {
      import scala.collection.JavaConversions._

      val returnVal = fileChooser.showOpenDialog(ASMEditorPane.this)
      if (returnVal != FileChooser.Result.Approve) return ;
      val file = fileChooser.selectedFile
      val is = new FileInputStream(file)
      val cr = new ClassReader(is)
      val node = new ClassNode()
      cr.accept(node, ClassReader.SKIP_DEBUG)
      methodList = node.methods.asInstanceOf[java.util.List[MethodNode]]
      methodComboBox = new ComboBox(methodList map { _.name })
      methodComboBox.peer.setAction(methodSelectAction.peer)
      methodSelector.contents(1) = methodComboBox
      methodSelectAction()
      // what follows does not work, probably for a scala bug..
      // therefore, we cannot change the combobox  component
      // val model = new DefaultComboBoxModel(methodList.toArray)
      // methodComboBox.peer.setModel(model: ComboBoxModel[_])
    }
  }

  def ensureSaved = true

  def updateFrameTitle() {
    val newTitle = softwareName + " - JVM"
    frame.title = newTitle
  }

  def analyze = {
    method match {
      case Some(method) =>
        try {
          val numericalDomain = frame.parametersPane.selectedNumericalDomain
          val params = new Parameters(method) { val domain = new JVMEnvFixedFrameDomain(numericalDomain) }
          frame.parametersPane.setParameters(params)
          val ann = method.analyze(params)
          Some(method.mkString(ann))
        } catch {
          case e: UnsupportedASMByteCodeException =>
            Dialog.showMessage(ASMEditorPane.this, e.getMessage + " : " + e.node, "Error in analysing bytecode", Dialog.Message.Error)
            None
          case e: Exception =>
            Dialog.showMessage(ASMEditorPane.this, e.getMessage, "Error", Dialog.Message.Error)
            e.printStackTrace()
            None           
        }
      case None => None
    }
  }

  val fileMenuItems = Seq(new MenuItem(newAction), new MenuItem(openAction))

  val editMenuItems = Seq()
  
  def select() = updateFrameTitle()

}