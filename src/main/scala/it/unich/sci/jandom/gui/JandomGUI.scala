package it.unich.sci.jandom
package gui

import javax.swing.JFileChooser
import domains.BoxDouble
import targets.slil.SLILStmt
import java.awt.event.ActionListener
import java.io.IOException
import java.io.FileReader
import java.io.FileWriter
import java.io.File
import javax.swing.event.DocumentListener
import javax.swing.JOptionPane
import it.unich.sci.jandom.targets

/**
 * The main GUI of Jandom.
 */
object JandomGUI extends App {
  val frame = new JandomFrame()
  frame.setVisible(true);
}
