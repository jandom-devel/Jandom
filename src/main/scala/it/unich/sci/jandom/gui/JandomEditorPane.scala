package it.unich.sci.jandom
package gui

import scala.swing._
import java.io._
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import java.awt.event.InputEvent

class JandomEditorPane extends EditorPane {
  private val fileChooser = new FileChooser(new File("."))
  private var modifiedSource = false
  private var currentFile: Option[File] = None
  clear()

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
      peer.write(new FileWriter(file))
      modifiedSource = false
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
    if (! modifiedSource) return true
    val retval = Dialog.showConfirmation(this, "The current file has meed modified. Do you want to save it?",
      "Save Resource", Dialog.Options.YesNoCancel, Dialog.Message.Question)
    retval match {
      case Dialog.Result.Yes => doSave(false)
      case Dialog.Result.No => true
      case Dialog.Result.Cancel => false
    }
  }

  /**
   * Set the DocumentListener for the Editor. It should be called after every method which change
   * the current Document.
   */
  private def setDocumentListener() {
    peer.getDocument().addDocumentListener(new DocumentListener {
      def changedUpdate(e: DocumentEvent) { listen(e) };
      def insertUpdate(e: DocumentEvent) { listen(e) };
      def removeUpdate(e: DocumentEvent) { listen(e) };
      def listen(e: DocumentEvent) {
        modifiedSource = true
      }
    })
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
    text = ""
    currentFile = None
    modifiedSource = false
    setDocumentListener()
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
      peer.read(new FileReader(file), file)
    } catch {
      case e: IOException =>
    }
    currentFile = Some(file)
    modifiedSource = false
    setDocumentListener()
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
    def apply() { open() }
  } 
}