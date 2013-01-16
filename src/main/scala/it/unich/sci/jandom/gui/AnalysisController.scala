package it.unich.sci.jandom
package gui

import javax.swing.JFrame
import javax.swing.text.JTextComponent

class AnalysisController(val sf: JTextComponent, val of: JTextComponent) {

  /**
   * Analyze.
   */
  def analyze() {
    val source = sf.getText()
    val parsed = parsers.RandomParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val domain = domains.BoxDouble
      val params = new targets.Parameters[domains.BoxDouble, targets.slil.SLILStmt](domain, program)
      val ann = program.analyze(params)
      of.setText(program.mkString(ann))
    }
  }
}