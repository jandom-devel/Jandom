package it.unich.sci.jandom
package parameters

/**
 * This objects determines the scope for widenings. The available alternatives are:
 * - Output: standard application of widening at the exit of join nodes
 * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
 * - Random: the scope used on Random. Widening is applied at the exit of join nodes, but join is only
 *           applied once.
 * At the moment, this is only supported by the SLIL target.
 */
object WideningScope extends ParameterEnumeration("Widening Scope", "The Widening scope") {
  type WideningScope = Value
  val Output = Value("Output", "The standard widening, which is applied to the output edge")
  val BackEdges = Value("Back Edges", "The widening is applied at the input back edges")
  val Random = Value("Random", "The widening is applied like in Random (a variant of Back Edge)")
}
