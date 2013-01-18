package it.unich.sci.jandom.gui.options

import scala.collection.mutable.ArrayBuffer

/**
 * An OptionDescription is the description of a generic option selectable
 * by the user.
 * @tparam T the type of the choices corresponding to this option
 * @param name the name of this option
 * @param description the description to be used in tooltips
 */
class OptionDescription[T] (val name: String, val description: String) {  
  /**
   * The possible values for this option
   */
  var values = ArrayBuffer[OptionValueDescription[T]]() 
}
