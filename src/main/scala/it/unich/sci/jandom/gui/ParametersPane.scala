package it.unich.sci.jandom
package gui

import domains.{ NumericalDomain, NumericalProperty }
import scala.swing._
import java.awt.GridBagConstraints
import scala.swing.ListView.Renderer
import it.unich.sci.jandom.targets.{Parameters,Target}
import it.unich.sci.jandom.parameters.Parameter
import it.unich.sci.jandom.parameters.ParameterEnumeration
import it.unich.sci.jandom.parameters.ParameterValue
import it.unich.sci.jandom.parameters.WideningScope
import it.unich.sci.jandom.parameters.NarrowingStrategy

class ParametersPane extends GridBagPanel {
  border = Swing.EmptyBorder(5, 5, 5, 5)
  val domainComboBox = addParameterEnumeration(0, NumericalDomain)
  val wideningComboBox = addParameterEnumeration(1, WideningScope)
  val narrowingComboBox = addParameterEnumeration(2, NarrowingStrategy)
  layout(Swing.VGlue) = new Constraints(0, 3, 2, 1, 0.0, 1.0, GridBagConstraints.BASELINE,
      GridBagConstraints.NONE, new Insets(0,0,0,0), 0, 0)
  
   object ParameterRenderer extends Renderer[ParameterValue] {
    val r = implicitly[Renderer[String]]
    def componentFor(list: ListView[_], isSelected: Boolean,
      focused: Boolean, a: ParameterValue, index: Int): Component =
      {
        val c = r.componentFor(list, isSelected, focused, a.name, index)
        c.tooltip = a.description
        return c
      }
  }
  
  private def addParameterEnumeration(row: Int, pe: Parameter[_]): ComboBox[_] = {
    val label = new Label(pe.name + ":") {
      tooltip = pe.description
    }
    val comboBox = new ComboBox(pe.enabledValues) {
      renderer = ParameterRenderer
    }
    layout(label) = new Constraints(0, row, 1, 1, 0.0, 0.0, GridBagConstraints.EAST,
      GridBagConstraints.NONE, new Insets(0, 0, 5, 5), 0, 0)
    layout(comboBox) = new Constraints(1, row, 1, 1, 1.0, 0.0, GridBagConstraints.WEST,
      GridBagConstraints.HORIZONTAL, new Insets(0, 0, 5, 0), 0, 0)
    comboBox
  }

  def selectedDomain = NumericalDomain.enabledValues(domainComboBox.selection.index)
  
  def getParameters[T <: Target](tgt: T) = {
    val parameters = new Parameters(selectedDomain, tgt)
    parameters.wideningScope = WideningScope(wideningComboBox.selection.index)
    parameters.narrowingStrategy = NarrowingStrategy(narrowingComboBox.selection.index)
    parameters
  }
}
