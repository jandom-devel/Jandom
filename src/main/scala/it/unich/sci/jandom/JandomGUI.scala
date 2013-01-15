package it.unich.sci.jandom

import gui._
import scala.swing._
import domains.BoxDouble
import targets.slil.SLILStmt

object JandomGUI extends App {
	val frame = new JandomFrame();
	
	val actionCommand = new Action("ANALYZE!!") {
	  def apply() { 
	    val source = frame.getEditorPane.getText
	    val parsed = parsers.RandomParser().parseProgram(source)
        if (parsed.successful) {
        	val program = parsed.get
        	val domain =  domains.BoxDouble
        	val params = new targets.Parameters[BoxDouble,SLILStmt](domain, program)
        	val ann = program.analyze(params)
        	frame.getOutputPane.setText(program.mkString(ann))
        	frame.switchToTab(JandomFrame.Tabs.OUTPUT)
        }
	  }
	}
	
	frame.getBtnAnalyze.setAction(actionCommand.peer)
	frame.setVisible(true);
}
