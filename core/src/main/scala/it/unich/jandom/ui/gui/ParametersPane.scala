/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import java.awt.GridBagConstraints

import scala.swing._
import scala.swing.ListView.Renderer

import it.unich.jandom.ui.ParameterValue
import it.unich.jandom.targets._
import it.unich.jandom.ui._
import it.unich.jandom.targets.parameters.WideningSpecs._
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import javax.swing.JSpinner
import javax.swing.SpinnerNumberModel

class ParametersPane extends GridBagPanel {
  border = Swing.EmptyBorder(5, 5, 5, 5)
  val numericalDomainComboBox = addParameterEnumeration(0, NumericalDomains)
  val objectDomainComboBox = addParameterEnumeration(1, ObjectDomains)
  val wideningComboBox = addParameterEnumeration(2, WideningScopes)
  val narrowingComboBox = addParameterEnumeration(3, NarrowingStrategies)
  val delayModel = new SpinnerNumberModel(0, 0, Double.PositiveInfinity, 1)
  val delay = Component.wrap(new JSpinner(delayModel))
  val debug = new CheckBox("Debug")

  layout(new Label("Widening Delay:")) = new Constraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.EAST,
    GridBagConstraints.NONE, new Insets(0, 0, 5, 5), 0, 0)
  layout(delay) = new Constraints(1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST,
    GridBagConstraints.HORIZONTAL, new Insets(0, 0, 5, 0), 0, 0)
  layout(debug) = new Constraints(0, 5, 2, 1, 0.0, 0.0, GridBagConstraints.BASELINE,
    GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0)
  layout(Swing.VGlue) = new Constraints(0, 6, 2, 1, 0.0, 1.0, GridBagConstraints.BASELINE,
    GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0)

  object ParameterRenderer extends Renderer[ParameterValue[_]] {
    private val r = implicitly[Renderer[String]]
    def componentFor(list: ListView[_ <: ParameterValue[_]], isSelected: Boolean,
      focused: Boolean, a: ParameterValue[_], index: Int): Component =
      {
        // The asInstanceOf in the line below is a bad trick... it works for now.
        val c = r.componentFor(list.asInstanceOf[ListView[String]], isSelected, focused, a.name, index)
        c.tooltip = a.description
        c
      }
  }

  private def addParameterEnumeration[V](row: Int, pe: ParameterEnumeration[V]): ComboBox[ParameterValue[V]] = {
    val label = new Label(pe.name + ":") {
      tooltip = pe.description
    }
    val comboBox = new ComboBox(pe.values) {
      renderer = ParameterRenderer
      selection.item = pe.default
    }
    layout(label) = new Constraints(0, row, 1, 1, 0.0, 0.0, GridBagConstraints.EAST,
      GridBagConstraints.NONE, new Insets(0, 0, 5, 5), 0, 0)
    layout(comboBox) = new Constraints(1, row, 1, 1, 1.0, 0.0, GridBagConstraints.WEST,
      GridBagConstraints.HORIZONTAL, new Insets(0, 0, 5, 0), 0, 0)
    comboBox
  }

  def selectedNumericalDomain = NumericalDomains.values(numericalDomainComboBox.selection.index).value

  def selectedObjectDomain = ObjectDomains.values(objectDomainComboBox.selection.index).value

  def setParameters[T <: Target[T]](params: Parameters[T]): Unit = {
    params.wideningScope = WideningScopes.values(wideningComboBox.selection.index).value
    params.narrowingStrategy = NarrowingStrategies.values(narrowingComboBox.selection.index).value
    val delay = delayModel.getValue().asInstanceOf[Double].toInt
    params.widening = DelayedWidening(DefaultWidening, delay)
    params.narrowing = DelayedNarrowing(TrivialNarrowing, 2)
    if (debug.selected) params.debugWriter = new java.io.StringWriter
  }

}
