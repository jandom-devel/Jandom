/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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
import java.io.{ File, FileWriter, IOException }
import scala.Array.canBuildFrom
import scala.swing.{ Action, Dialog, EditorPane, FileChooser, MenuItem, Separator }
import scala.swing.ScrollPane
import it.unich.jandom._
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.slil.SLILTarget
import javax.swing.KeyStroke
import javax.swing.event.{ DocumentEvent, DocumentListener, UndoableEditEvent, UndoableEditListener }
import javax.swing.undo.UndoManager
import it.unich.jandom.targets.slil.SLILPrinterSpecOffline
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.nio.file.Files
import it.unich.jandom.ui.HTMLOutput

class JandomEditorPane(val frame: MainFrame) extends ScrollPane with TargetPane {
  val editorPane = new EditorPane
  contents = editorPane

  val fileChooser = new FileChooser(new File("."))
  val actionMap = Map(editorPane.peer.getActions() map
    { action => action.getValue(javax.swing.Action.NAME) -> action }: _*)
  var undo = new UndoManager
  var _currentFile: Option[File] = None
  var _modified = false

  def modified = _modified
  def modified_=(v: Boolean) {
    _modified = v
    updateFrameTitle()
  }

  def currentFile = _currentFile
  def currentFile_=(f: Option[File]) {
    _currentFile = f
    updateFrameTitle()
  }

  editorPane.peer.getDocument().addDocumentListener(new DocumentListener {
    def changedUpdate(e: DocumentEvent) { listen(e) };
    def insertUpdate(e: DocumentEvent) { listen(e) };
    def removeUpdate(e: DocumentEvent) { listen(e) };
    def listen(e: DocumentEvent) {
      modified = true
    }
  })

  editorPane.peer.getDocument().addUndoableEditListener(new UndoableEditListener {
    def undoableEditHappened(e: UndoableEditEvent) {
      undo.addEdit(e.getEdit())
      undoAction.updateUndoState()
      redoAction.updateRedoState()
    }
  })

  def updateFrameTitle() {
    val newTitle = softwareName +
      (currentFile match {
        case None => ""
        case Some(f) => " - " + f.getName()
      }) +
      (modified match {
        case true => " (modified) "
        case false => ""
      })
    frame.title = newTitle
  }

  /**
   * Saves the current file. It asks for the filename if it is not known or if
   * forceDialog is true. Returns true if saving succeeds.
   */
  private def doSave(forceDialog: Boolean): Boolean = {
    val file = (currentFile, forceDialog) match {
      case (Some(f), false) => f
      case _ =>
        val returnVal = fileChooser.showSaveDialog(this);
        if (returnVal == FileChooser.Result.Approve)
          fileChooser.selectedFile
        else
          return false
    }
    try {
      editorPane.peer.write(new FileWriter(file))
      modified = false
      currentFile = Some(file)
    } catch {
      case e: IOException => return false
    }
    return true
  }

  /**
   * Asks whether the user wants to save the current editor content, and save it in case. Returns false
   * if the current action should be cancelled.
   */
  def ensureSaved(): Boolean = {
    if (!modified) return true
    val retval = Dialog.showConfirmation(this, "The current file has meed modified. Do you want to save it?",
      "Save Resource", Dialog.Options.YesNoCancel, Dialog.Message.Question)
    retval match {
      case Dialog.Result.Yes => doSave(false)
      case Dialog.Result.No => true
      case Dialog.Result.Cancel => false
    }
  }

  /**
   * Save the current editor content, with the old filename if present.
   */
  def save() { doSave(false) }

  /**
   * Save the current editor content, with a new filename.
   */
  def saveAs() { doSave(true) }

  /**
   * Clear the current content.
   */
  def clear() {
    if (!ensureSaved()) return ;
    currentFile = None
    modified = false
  }

  /**
   * Open a new file.
   */
  def open() {
    if (!ensureSaved()) return ;
    val returnVal = fileChooser.showOpenDialog(this)
    if (returnVal != FileChooser.Result.Approve) return ;
    val file = fileChooser.selectedFile
    try {
      editorPane.text = scala.io.Source.fromFile(file).mkString
    } catch {
      case e: IOException =>
    }
    currentFile = Some(file)
    modified = false
  }

  val newAction = new Action("New") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK))
    def apply() { clear() }
  }

  val openAction = new Action("Open...") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK))
    def apply() { open() }
  }

  val saveAction = new Action("Save") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK))
    def apply() { save() }
  }

  val saveAsAction = new Action("Save As") {
    def apply() { saveAs() }
  }

  val cutAction = new Action("Cut") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_MASK))
    def apply() { actionMap("cut-to-clipboard").actionPerformed(null) }
  }

  val pasteAction = new Action("Paste") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_V, InputEvent.CTRL_MASK))
    def apply() { actionMap("paste-from-clipboard").actionPerformed(null) }
  }

  val copyAction = new Action("Copy") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK))
    def apply() { actionMap("copy-to-clipboard").actionPerformed(null) }
  }

  val outputHTMLAction = new Action("HTML output") {
    def apply() { outputHTML() }
  }

  object undoAction extends Action("Undo") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_MASK))

    def apply() {
      undo.undo()
      updateUndoState()
      redoAction.updateRedoState()
    }

    def updateUndoState(): Unit = {
      if (undo.canUndo()) {
        enabled = true
        title = undo.getUndoPresentationName()
      } else {
        enabled = false
        title = "Undo"
      }
    }
  }

  object redoAction extends Action("Redo") {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK))

    def apply() {
      undo.redo()
      updateRedoState()
      undoAction.updateUndoState()
    }

    def updateRedoState(): Unit = {
      if (undo.canRedo()) {
        enabled = true
        title = undo.getRedoPresentationName()
      } else {
        enabled = false
        title = "Redo"
      }
    }
  }

  def analyze = {
    val parser = parsers.RandomParser()
    parser.parseProgram(editorPane.text) match {
      case parser.Success(program, _) =>
        val numericalDomain = frame.parametersPane.selectedNumericalDomain
        val params = new Parameters[SLILTarget] { val domain = numericalDomain }
        frame.parametersPane.setParameters(params)
        val ann = program.analyze(params)
        Some(params.debugWriter.toString + program.mkString(ann))
      case parser.NoSuccess(msg, next) =>
        Dialog.showMessage(JandomEditorPane.this, msg + " in line " + next.pos.line + " column " + next.pos.column,
          "Error in parsing source code", Dialog.Message.Error)
        None
    }
  }

  def outputHTML() {
    val parser = parsers.RandomParser()
    parser.parseProgram(editorPane.text) match {
      case parser.Success(program, _) =>
        val numericalDomain = frame.parametersPane.selectedNumericalDomain
        val params = new Parameters[SLILTarget] { val domain = numericalDomain }
        frame.parametersPane.setParameters(params)
        val ann = program.analyze(params)
        val ppspec = SLILPrinterSpecOffline(program.env)
        val out = program.mkString(ann, ppspec)
        val HTMLout = HTMLOutput(out, ppspec.annotations)
        Files.write(Paths.get("output.html"), HTMLout.getBytes(StandardCharsets.UTF_8))
      case parser.NoSuccess(msg, next) =>
        Dialog.showMessage(JandomEditorPane.this, msg + " in line " + next.pos.line + " column " + next.pos.column,
          "Error in parsing source code", Dialog.Message.Error)
        None
    }
  }

  val fileMenuItems = Seq(new MenuItem(newAction), new MenuItem(openAction), new Separator, new MenuItem(saveAction),
    new MenuItem(saveAsAction), new Separator, new MenuItem(outputHTMLAction))
  val editMenuItems = Seq(new MenuItem(undoAction), new MenuItem(redoAction), new Separator,
    new MenuItem(cutAction), new MenuItem(copyAction), new MenuItem(pasteAction))

  def select() = updateFrameTitle()
}
