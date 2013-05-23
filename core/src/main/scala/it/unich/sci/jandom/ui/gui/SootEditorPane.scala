/**
 * Copyright 2013 amato
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
import java.io.File
import scala.collection.JavaConversions._
import scala.swing.{Action, BorderPanel, BoxPanel, ComboBox, EditorPane, FileChooser, Label, MenuItem, Orientation, ScrollPane}
import scala.swing.Dialog
import it.unich.sci.jandom._
import it.unich.sci.jandom.ppfactories.MemoizingFactory
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.targets.Parameters
import it.unich.sci.jandom.targets.jvm._
import javax.swing.KeyStroke
import soot.Scene
import soot.SootMethod
import it.unich.sci.jandom.domains.objects.ObjectNumericalDomain

/**
 * This is the pane used to select the class and method to analyze for
 * the Soot Baf and Jimple targets.
 * @author Gianluca Amato
 *
 */

class SootEditorPane(val frame: MainFrame) extends BorderPanel with TargetPane {
  import frame.Mode._

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
  private var methodList = Seq[SootMethod]()

  /*
   * The method currently selected, or None if no method is selected.
   */
  private var method: Option[Target[_]] = None

  editorPane.editable = false
  layout(new ScrollPane(editorPane)) = BorderPanel.Position.Center
  layout(methodSelector) = BorderPanel.Position.North

  val methodSelectAction = new Action("Method Select") {
    def apply() {
      val methodName = methodComboBox.selection.item
      val sootMethod = methodList.find(_.getName == methodName)
      method = sootMethod flatMap { sootMethod =>
        frame.mode match {
        	case Baf => Some(new BafMethod(sootMethod))
            case Jimple => Some(new JimpleMethod(sootMethod))
        }
      }
      editorPane.text = method match {
        case Some(method) => method.toString
        case None => ""
      }
    }
  }

  val newAction = new Action("New") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK))
    def apply() { editorPane.text = "" }
  }

  val openAction = new Action("Open...") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK))
    def apply() {
      val returnVal = fileChooser.showOpenDialog(SootEditorPane.this)
      if (returnVal != FileChooser.Result.Approve) return ;
      val file = fileChooser.selectedFile
      val fileName = file.getName
      val (name, extension) = fileName.splitAt(fileName.lastIndexOf("."))

      val scene = Scene.v()
      scene.setSootClassPath(scene.defaultClassPath + ":" + file.getParent())
      val c = scene.loadClass(name, 1)
      c.setApplicationClass()

      methodList = c.getMethods()
      methodComboBox = new ComboBox(methodList map { _.getName })
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
          frame.mode match {
            case Baf =>
              val bafMethod = method.asInstanceOf[BafMethod]
              val params = new Parameters[BafMethod] { val domain = new JVMEnvDynFrameDomain(numericalDomain) }
              frame.parametersPane.setParameters(params)
              params.wideningFactory = MemoizingFactory(bafMethod)(params.wideningFactory)
              params.narrowingFactory = MemoizingFactory(bafMethod)(params.narrowingFactory)
              val ann = bafMethod.analyze(params)
              Some(bafMethod.mkString(ann))
            case Jimple =>
              val jimpleMethod = method.asInstanceOf[JimpleMethod]
              val params = new Parameters[JimpleMethod]{ val domain = new ObjectNumericalDomain(numericalDomain) }
              frame.parametersPane.setParameters(params)
              params.wideningFactory = MemoizingFactory(jimpleMethod)(params.wideningFactory)
              params.narrowingFactory = MemoizingFactory(jimpleMethod)(params.narrowingFactory)
              val ann = jimpleMethod.analyze(params)
              Some(jimpleMethod.mkString(ann))
          }
        } catch {
          case e: UnsupportedBafByteCodeException =>
            println(e.getMessage)
            Dialog.showMessage(SootEditorPane.this, e.getMessage + " : " + e.inst, "Error in analysing bytecode", Dialog.Message.Error)
            None
          case e: Exception =>
            Dialog.showMessage(SootEditorPane.this, e.getMessage, "Error in parsing source code", Dialog.Message.Error)
            e.printStackTrace()
            None
        }
      case None => None
    }
  }

  val fileMenuItems = Seq(new MenuItem(newAction), new MenuItem(openAction))

  val editMenuItems = Seq()

  def select = {
    methodSelectAction()
    updateFrameTitle()
  }
}