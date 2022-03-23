/**
  * Copyright 2013, 2016, 2018 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.jandom.ui.gui

import java.awt.event.InputEvent
import java.awt.event.KeyEvent
import java.io.File
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import scala.jdk.CollectionConverters._
import scala.swing._
import scala.swing.event.ActionEvent
import scala.swing.event.EditDone
import scala.swing.event.SelectionChanged
import scala.util.Try

import it.unich.jandom._
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.jvmsoot._
import it.unich.jandom.ui.OutputInterface
import it.unich.jandom.ui.gui.MainFrame
import javax.swing.KeyStroke
import soot.Scene
import soot.SootMethod

/**
  * This is the pane used to select the class and method to analyze for
  * the Soot Baf and Jimple targets.
  *
  * @author Gianluca Amato
  *
  */

class SootEditorPane(val frame: MainFrame) extends BorderPanel with TargetPane {

  private val sootScene = Scene.v()
  sootScene.loadBasicClasses()

  private val editorPane = new EditorPane
  editorPane.editable = false
  private val classPathField = new TextField(new File("examples/Java").getCanonicalPath)
  private val classComboBox = new ComboBox(Seq[String]())
  private val methodComboBox = new ComboBox(Seq[SootMethod]())
  private val radioBaf = new RadioButton("Baf")
  radioBaf.tooltip = OutputInterface.getRadioBafTip
  private val radioJimple = new RadioButton("Jimple")
  radioJimple.tooltip = OutputInterface.getRadioJimpleTip
  private val typeGroup = new ButtonGroup(radioBaf, radioJimple)
  typeGroup.select(radioJimple)
  private val radioNumerical = new RadioButton("Numerical")
  radioNumerical.tooltip = OutputInterface.getRadioNumericalTip
  private val radioObject = new RadioButton("Object")
  radioObject.tooltip = OutputInterface.getRadioObjectTip
  private val anGroup = new ButtonGroup(radioNumerical, radioObject)
  anGroup.select(radioNumerical)

  var optMethod: Option[SootCFG[_, _]] = None

  val controls: GridBagPanel = new GridBagPanel {
    val c = new Constraints

    c.weightx = 0
    c.gridx = 0
    c.gridy = 0
    val cplabel = new Label("ClassPath: ")
    cplabel.tooltip = OutputInterface.getClassPathTip
    layout(cplabel) = c

    c.gridy = 1
    val clabel = new Label("Class: ")
    clabel.tooltip = OutputInterface.getClassTip
    layout(clabel) = c

    c.gridy = 2
    val mlabel = new Label("Method: ")
    mlabel.tooltip = OutputInterface.getMethodTip

    layout(mlabel) = c

    c.fill = GridBagPanel.Fill.Horizontal
    c.weightx = 80
    c.gridx = 1
    c.gridy = 0
    layout(classPathField) = c

    c.gridy = 1
    layout(classComboBox) = c

    c.gridy = 2
    layout(methodComboBox) = c

    c.gridy = 3
    c.gridx = 0
    c.gridwidth = 2
    val horPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {

      val tlabel = new Label("IR type: ")
      tlabel.tooltip = OutputInterface.getIRTypeTip

      val anlabel = new Label("Analysis type: ")
      anlabel.tooltip = OutputInterface.getAnalysisTypeTip

      contents += Swing.HGlue += tlabel += radioBaf += radioJimple +=
        Swing.HStrut(100) +=
        anlabel += radioNumerical += radioObject +=
        Swing.HGlue

    }
    layout(horPanel) = c

  }
  layout(new ScrollPane(editorPane)) = BorderPanel.Position.Center
  layout(controls) = BorderPanel.Position.North

  listenTo(classPathField, classComboBox.selection, methodComboBox.selection, radioBaf, radioJimple)
  reactions += {
    case EditDone(`classPathField`) =>
      sootScene.setSootClassPath(sootScene.defaultClassPath + java.io.File.pathSeparator + classPathField.text)
      val rootPath = Paths.get(classPathField.text)
      val fileProcessor = new ClassFileVisitor(rootPath)
      if (Try(Files.walkFileTree(rootPath, fileProcessor)).isSuccess) {
        val comboModel = ComboBox.newConstantModel(fileProcessor.classNameList)
        classComboBox.peer.setModel(comboModel)
        publish(SelectionChanged(classComboBox))
        classPathField.foreground = java.awt.Color.black
      } else
        classPathField.foreground = java.awt.Color.red

    case SelectionChanged(`classComboBox`) =>
      val klass = sootScene.loadClassAndSupport(classComboBox.selection.item)
      val methodList = klass.getMethods.asScala
      // these two lines are a mess because Scala Swing does not play well with Java 1.7
      val comboModel = ComboBox.newConstantModel(methodList)
      methodComboBox.peer.asInstanceOf[javax.swing.JComboBox[SootMethod]].setModel(comboModel)
      publish(SelectionChanged(methodComboBox))

    case SelectionChanged(`methodComboBox`) | ActionEvent(`radioBaf`) | ActionEvent(`radioJimple`) =>
      val sootMethod = methodComboBox.selection.item
      optMethod = typeGroup.selected match {
        case Some(`radioBaf`) => Some(new BafMethod(sootMethod, false))
        case Some(`radioJimple`) => Some(new JimpleMethod(sootMethod, false))
        case _ => None
      }
      editorPane.text = optMethod match {
        case None => ""
        case Some(m) => m.toString
      }
  }

  publish(EditDone(classPathField))

  class ClassFileVisitor(rootPath: Path) extends SimpleFileVisitor[Path] {
    private val privateClassNamesList = scala.collection.mutable.SortedSet[String]()

    def classNameList: Seq[String] = privateClassNamesList.toSeq

    override def visitFile(aFile: Path, aAttrs: BasicFileAttributes): FileVisitResult = {
      val relativePath = rootPath.relativize(aFile).asScala
      val className = (relativePath.tail foldLeft relativePath.head.toString) (_ + "." + _.toString)
      if (className endsWith ".class")
        privateClassNamesList += className stripSuffix ".class"
      else if (className endsWith ".java")
        privateClassNamesList += className stripSuffix ".java"
      FileVisitResult.CONTINUE
    }
  }

  val openAction: Action = new Action("Change classpath...") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_DOWN_MASK))

    def apply() = {
      val fileChooser = new FileChooser(new File(classPathField.text))
      fileChooser.title = "Select classpath"
      fileChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      val returnVal = fileChooser.showOpenDialog(SootEditorPane.this)
      if (returnVal == FileChooser.Result.Approve) {
        val file = fileChooser.selectedFile
        classPathField.text = file.getPath
        publish(EditDone(classPathField))
      }
    }
  }

  def ensureSaved() = true

  def analyze: Option[String] = {
    optMethod match {
      case Some(method) =>
        try {
          val numericalDomain = frame.parametersPane.selectedNumericalDomain
          val objectDomain = frame.parametersPane.selectedObjectDomain
          val om = new SootObjectModel(sootScene)
          val sootDomain = if (anGroup.selected.contains(radioNumerical))
            new SootFrameNumericalDomain(numericalDomain)
          else
            new SootFrameObjectDomain(objectDomain(om))
          typeGroup.selected match {
            case Some(`radioBaf`) =>
              val bafMethod = method.asInstanceOf[BafMethod]
              val params = new Parameters[BafMethod] {
                val domain: BafMethod#DomainBase = sootDomain
              }
              frame.parametersPane.setParameters(params)
              val inte = new TopSootInterpretation[BafMethod, params.type](params)
              params.interpretation = Some(inte)
              val ann = bafMethod.analyze(params)
              Some(bafMethod.mkString(params)(ann))
            case Some(`radioJimple`) =>
              val jimpleMethod = method.asInstanceOf[JimpleMethod]
              val params = new Parameters[JimpleMethod] {
                val domain: JimpleMethod#DomainBase = sootDomain
              }
              frame.parametersPane.setParameters(params)
              val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
              params.interpretation = Some(inte)
              val ann = jimpleMethod.analyze(params)
              Some(jimpleMethod.mkString(params)(ann))
            case _ => None
          }
        } catch {
          case e: UnsupportedSootUnitException =>
            Dialog.showMessage(SootEditorPane.this, e.getMessage + " : " + e.unit, "Error in analysing bytecode", Dialog.Message.Error)
            e.printStackTrace()
            None
          case e: Exception =>
            Dialog.showMessage(SootEditorPane.this, e.getMessage, "Error in parsing source code", Dialog.Message.Error)
            e.printStackTrace()
            None
        }
      case _ => None
    }
  }

  val fileMenuItems: Seq[MenuItem] = Seq(new MenuItem(openAction))

  val editMenuItems: Seq[Nothing] = Seq()

  def select() = {
    val newTitle = softwareName + " - Soot"
    frame.title = newTitle
  }
}
