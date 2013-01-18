package it.unich.sci.jandom
package gui

import domains.{ NumericalDomain, NumericalProperty }
import options._
import scala.swing._
import java.awt.GridBagConstraints
import scala.swing.ListView.Renderer
import it.unich.sci.jandom.targets.{Parameters,Target}

class ParametersPane extends GridBagPanel {
  border = Swing.EmptyBorder(5, 5, 5, 5)
  val domainComboBox = addOptionDescription(0, domainOptionDescription)
  val wideningComboBox = addOptionDescription(1, wideningScopeDescription)  
  
  object OptionValueDescriptionRenderer extends Renderer[OptionValueDescription[_]] {
    val r = implicitly[Renderer[String]]
    def componentFor(list: ListView[_], isSelected: Boolean,
      focused: Boolean, a: OptionValueDescription[_], index: Int): Component =
      {
        val c = r.componentFor(list, isSelected, focused, a.name, index)
        c.tooltip = a.description
        return c
      }
  }

  private def addOptionDescription[T](row: Int, od: OptionDescription[T]): ComboBox[OptionValueDescription[T]] = {
    val label = new Label(od.name + ":") {
      tooltip = od.description
    }
    val comboBox = new ComboBox(od.values) {
      renderer = OptionValueDescriptionRenderer      
    }

    layout(label) = new Constraints(0, row, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE,
      GridBagConstraints.NONE, new Insets(0, 0, 5, 5), 0, 0)
    layout(comboBox) = new Constraints(1, row, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE,
      GridBagConstraints.HORIZONTAL, new Insets(0, 0, 5, 0), 0, 0)
    comboBox
  }

  def selectedDomain = domainOptionDescription.values(domainComboBox.selection.index).value
  
  def getParameters[T <: Target](tgt: T) = {
    val parameters = new Parameters(selectedDomain, tgt)
    parameters.wideningScope = wideningScopeDescription.values(wideningComboBox.selection.index).value
    parameters
  }
}
