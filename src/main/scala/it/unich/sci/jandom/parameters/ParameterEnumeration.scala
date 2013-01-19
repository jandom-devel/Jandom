package it.unich.sci.jandom.parameters

class ParameterEnumeration(val name: String, val description: String = "") extends Enumeration with Parameter[ParameterValue] {
  class Val(val name: String, val description: String) extends super.Val(name) with ParameterValue
  protected final def Value(name: String = "", description : String = ""): Value = new Val(name, description)
  def enabledValues =  values.toSeq.asInstanceOf[Seq[Val]] 
}
