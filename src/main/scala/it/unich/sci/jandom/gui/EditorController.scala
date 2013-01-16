package it.unich.sci.jandom
package gui

import javax.swing.text.JTextComponent
import javax.swing.JFileChooser
import java.io.File
import java.io.FileWriter
import java.io.IOException
import javax.swing.JOptionPane
import java.io.FileReader
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent

/**
 * This is the controller for the Editor tab in the GUI.
 */
class EditorController(private val pane: JTextComponent) {
  private val fileChooser = new JFileChooser(System.getProperty("user.dir"))
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
        val returnVal = fileChooser.showSaveDialog(pane);
        if (returnVal == JFileChooser.APPROVE_OPTION)
          fileChooser.getSelectedFile()
        else
          return false
    }
    try {
      pane.write(new FileWriter(file))
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
  private def askConfirmation(): Boolean = {
    val retval = JOptionPane.showConfirmDialog(pane, "The current file has meed modified. Do you want to save it?",
      "Save Resource", JOptionPane.YES_NO_CANCEL_OPTION,
      JOptionPane.QUESTION_MESSAGE, null)
    retval match {
      case JOptionPane.YES_OPTION => doSave(false)
      case JOptionPane.NO_OPTION => true
      case JOptionPane.CANCEL_OPTION => false
    }
  }

  /**
   * Set the DocumentListener for the Editor. It should be called after every method which change 
   * the current Document.
   */
  private def setDocumentListener() {
    pane.getDocument().addDocumentListener(new DocumentListener {
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
    if (modifiedSource && !askConfirmation()) return ;
    pane.setText("")
    currentFile = None
    modifiedSource = false
    setDocumentListener()
  }

  /**
   * Open a new file.
   */
  def open() {
    if (modifiedSource && !askConfirmation()) return ;
    val returnVal = fileChooser.showOpenDialog(pane);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      val file = fileChooser.getSelectedFile()
      try {
        pane.read(new FileReader(file), file)
      } catch {
        case e: IOException =>
      }
      currentFile = Some(file)
      modifiedSource = false
      setDocumentListener()
    }
  }
  
  /**
   * Returns the current text of the editor
   */
  def text = pane.getText()
}
