package it.unich.sci.jandom.parameters

/**
 * Every possible value of an enumeration parameter should implement
 * the trai ParameterValue 
 */
trait ParameterValue {
  /**
   * The name of the parameter value, to appear in UI
   */
  val name: String
  
  /**
   * A description of the parameter to be used, for example, in tooltips
   */
  val description: String
}
