package it.unich.sci.jandom.parameters

/** 
 * Every parameter which may be used in the analyzer should mix the
 * trait Parameter.
 */
trait Parameter[V] {
  /**
   * The name of the parameter, to appear in the UI
   */
  val name: String
  
  /**
   * A description of the parameter to be used, for example, in tooltips
   */
  val description: String  
  
  /**
   * A list of enabled values for this parameter
   */
  def enabledValues: Seq[V with ParameterValue]
}
