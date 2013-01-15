package it.unich.sci.jandom.targets

/**
 * This objects determines the scope for widenings. The available alternatives are:
 * - Output: standard application of widening at the exit of join nodes
 * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
 * - Random: the scope used on Random. Widening is applied at the exit of join nodes, but join is only
 *           applied once.
 * At the moment, this is only supported by the SLIL target.
 */
object WideningScope extends Enumeration {
	type WideningScope = Value
	val Output, BackEdges, Random = Value
}
