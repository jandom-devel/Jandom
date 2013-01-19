package it.unich.sci.jandom.parameters

/**
 * This parameter specifies when and whether to build descending sequences during the analysis. 
 * The available alternatives are:
 * - None: no decending steps are performed
 * - Separate: first all the ascendings steps are performed, then all descending steps
 * - Restart: the standard Random strategy of perfoming Narrowing intertwined with Widening
 * - Continue: similar to Restart, but during narrowing of outer loops, inner loops only performs narrowing
 * At the moment, this is only supported by the SLIL target.
 */
object NarrowingStrategy extends ParameterEnumeration("Narrowing Strategy",
    "This parameter specifies when and whether to build descending sequences during the analysis.") {
	type NarrowingStrategy = Value
	val None = Value("None","No narrowing is performed")
	val Separate = Value("Separate","Narrowing is performed at the end, after the ascending phase is concluded")
	val Restart = Value("Restart", "Narrowing in intertwined with ascending phase")
	val Continue = Value("Continue", "Similar to Restart, but after a node is descending, it never ascends again")
}
