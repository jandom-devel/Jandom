package it.unich.jandom.targets

/**
 * This parameter specifies when and whether to build descending sequences during the analysis. 
 * The available alternatives are:
 * - None: no decending steps are performed
 * - Separate: standard narrowing, where first all the ascendings steps are performed, then all descending steps
 * - Restart: localized narrowing with Restart strategy, as described in the paper submitted to SAS 2013
 * - Continue: localized narrowing with Continue strategy,  as described in the paper submitted to SAS 2013
 * At the moment, this is only supported by the SLIL target.
 */

object NarrowingStrategy extends Enumeration {   
	type NarrowingStrategy = Value	
	val None = Value
	val Separate = Value
	val Restart = Value
	val Continue = Value	 
}
