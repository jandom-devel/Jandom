package it.unich.sci.jandom.targets

/**
 * This objects determines the scope for widenings. The available alternatives are:
 * - Output: standard application of widening at the exit of join nodes
 * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
 * - Random: localized widening as described in the paper submitted to SAS 2013.
 * At the moment, this is only supported by the SLIL target.
 */

object WideningScope extends Enumeration {  
  type WideningScope = Value
  val Output = Value
  val BackEdges = Value
  val Random = Value  
}
