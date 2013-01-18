package it.unich.sci.jandom.gui.options

/**
 * An OptionValueDescription is the description for a possible value for
 * an option selectable by the user
 * @tparam T the type of the value corresponding to this option
 * @param name the name of the option
 * @param description the description to be used in tooltips
 * @param value the value of this option
 */

class OptionValueDescription[+T] (val name: String, val description: String, val value: T) { }  