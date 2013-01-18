package it.unich.sci.jandom.gui

import it.unich.sci.jandom.domains._
import it.unich.sci.jandom.targets.WideningScope

/**
 * This package contains a description of the options available in the Jandom UI,
 * to be used by user interfaces.
 */
package object options {
  
  /**
   * This is the description of all the numerical domains we want to
   * consider in the GUI.
   */
  val domainOptionDescription = new OptionDescription[GenericNumericalDomain](
    "Domain",
    "The domain to be used for the analysis")

  domainOptionDescription.values += new OptionValueDescription[NumericalDomain[BoxDouble]](
    "Box Double",
    "A native scala implementation of boxes over double",
    BoxDouble)

  domainOptionDescription.values += new OptionValueDescription[NumericalDomain[PPLBoxDouble]](
    "PPL Double",
    "A PPL backed implementation of boxes over double",
    PPLBoxDouble)

  domainOptionDescription.values += new OptionValueDescription[NumericalDomain[PPLCPolyhedron]](
    "PPL C Polyehdra",
    "A  PPL backed implementation of closed polyhedra over double",
    PPLCPolyhedron)
    
  val wideningScopeDescription = new OptionDescription[WideningScope.Value](
      "Widening Scope",
      "The widening scope to be used for the analysis"
      )      

  wideningScopeDescription.values += new OptionValueDescription[WideningScope.Value](
      "Output",
      "The standard way of applying widening, at the output of union nodes",
      WideningScope.Output)
      
  wideningScopeDescription.values += new OptionValueDescription[WideningScope.Value](
      "BackEdges",
      "The optimized way of applying widening, at the backedge input of union nodesa",
      WideningScope.Output)

  wideningScopeDescription.values += new OptionValueDescription[WideningScope.Value](
      "Random",
      "The optimized way of applying widening used in Random",
      WideningScope.Output)      
      
}
