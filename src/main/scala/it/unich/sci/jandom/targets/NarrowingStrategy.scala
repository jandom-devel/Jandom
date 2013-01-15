package it.unich.sci.jandom.targets

/**
 * This ojects determines the strategy used for narrowings. The available alternatives are:
 * - None: no narrowing is performed
 * - Separate: first only widenings are perfomed, then all narrowings
 * - Restart: the standard Random strategy of perfoming Narrowing intertwined with Widening
 * - Continue: similar to Restart, but during narrowing of outer loops, inner loops only performs narrowing
 * At the moment, this is only supported by the SLIL target.
 */
object NarrowingStrategy extends Enumeration {
	type NarrowingStrategy = Value
	val None, Separate, Restart, Continue = Value
}
