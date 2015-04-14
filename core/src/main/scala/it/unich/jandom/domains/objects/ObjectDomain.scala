/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.objects

import it.unich.jandom.domains.CartesianFiberedDomain
import it.unich.jandom.domains.CartesianFiberedProperty
import it.unich.jandom.objectmodels.ObjectModel

/**
 * This trait represents the interface for a domain which handles objects and their relationship.
 * May be used, for example, for sharing analysis. This is only a draft, and will be probably improved
 * along the development of Jandom.
 * @tparam OM the class of object models related to this domain
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectDomain[+OM <: ObjectModel] extends CartesianFiberedDomain {
  /**
   * The property associated to an ObjectDomain is an ObjectProperty.
   */
  type Property <: ObjectProperty[Property]

  /**
   * The type of the fiber components corresponds to the type of the object model.
   */
  type FiberComponent = om.Type

  /**
   * The object model of this domain. An object model abstracts the details of the ways
   * a programming languages deals with objects.
   */
  val om: OM

  /**
   * This trait is the interface for abstract elements in the object domain.
   */
  trait ObjectProperty[P <: ObjectProperty[P]] extends CartesianFiberedProperty[om.Type, P] {
    this: P =>

    /**
     * Returns the type of an object reachable by following a sequence of fields. The
     * returned type should be a super-type of all the object possibly reachable with
     * this sequence of fields. Returns `None` if the field sequence stops due to a
     * null pointer.
     */
    def typeOf(v: Int, fs: Iterable[om.Field]): Option[om.Type]

    /**
     * Add a new variable. The new variable may be in whatever relationship with the
     * old ones.
     * @param t the type of the new variable
     */
    def addUnknownVariable(t: om.Type): P

    /**
     * Add a new non-null variable which does not share with any other variable.
     * @param t the type of the new variable
     */
    def addFreshVariable(t: om.Type): P

    /**
     * Assign the null object to variable `dst`.
     */
    def assignNull(dst: Int = dimension - 1): P

    /**
     * Corresponds to the assignment `dst = src`. We assume dst is a subtype
     * of src
     */
    def assignVariable(dst: Int, src: Int): P

    /**
     * Corresponds to the assignment `dst.field = src`. We assume dst.field is a subtype
     * of src
     */
    def assignVariableToField(dst: Int, field: om.Field, src: Int): P

    /**
     * Corresponds to the assignment `dst = src.field`. We assume dst is a subtype of src
     */
    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): P

    /**
     * Change the type of variable v-th. We assume the new type is comparable with the old one.
     */
    def castVariable(v: Int, newtype: om.Type): P

    /**
     * Returns true if the variable v might be null
     */
    def mustBeNull(v: Int): Boolean = mustBeNull(v, Iterable())

    /**
     * Returns true if the variable v must be null
     */
    def mayBeNull(v: Int): Boolean = mayBeNull(v, Iterable())

    /**
     * Returns true if the location obtained by v following fields in fieldseq is definitively
     * null. If some intermediate value is definitively null, it returns true.
     */
    def mustBeNull(v: Int, fieldseq: Iterable[om.Field]): Boolean

    /**
     * Returns true if the location obtained by v following fields in fieldseq may be
     * null. If some intermediate value is definitively null, it returns true.
     */
    def mayBeNull(v: Int, fieldseq: Iterable[om.Field]): Boolean

    /**
     * Returns true if two variables may share
     */
    def mayShare(v1: Int, v2: Int): Boolean = mayShare(v1, Iterable(), v2, Iterable())

    /**
     * Returns true if two fields may share
     */
    def mayShare(v1: Int, f1: Iterable[om.Field], v2: Int, f2: Iterable[om.Field]): Boolean

    /**
     * Returns true if two variables must share
     */
    def mustShare(v1: Int, v2: Int): Boolean = mustShare(v1, Iterable(), v2, Iterable())

    /**
     * Returns true if two fields must share
     */
    def mustShare(v1: Int, f1: Iterable[om.Field], v2: Int, f2: Iterable[om.Field]): Boolean

    /**
     * Returns true if two variables may be aliases, i.e. if they might point to the same
     * location.
     */
    def mayBeAliases(v1: Int, v2: Int): Boolean

    /**
     * Returns true if two variables must be aliases, i.e. they should point to the same
     * location.
     */
    def mustBeAliases(v1: Int, v2: Int): Boolean

    /**
     * Returns true if two variables may be weak aliases, i.e. they point to the same
     * location or are both null.
     */

    def mayBeWeakAliases(v1: Int, v2: Int): Boolean
    /**
     * Returns true if two variables must be weak aliases, i.e. they point to the same
     * location or are both null.
     */

    def mustBeWeakAliases(v1: Int, v2: Int): Boolean

    /**
     * Returns the result after the successful completion of the test `v == null`
     */
    def testNull(v: Int): P

    /**
     * Returns the result after the successful completion of the test `v != null`
     */
    def testNotNull(v: Int): P

    /**
     * The connect method is used for inter-procedural analysis. It takes two properties
     * such that the last `common` dimensions of `this` corresponds to the first `common`
     * dimension of `other`. The first represents the abstract state before calling a
     * procedure, the second represents the abstract state at the end of the procedure.
     * `connect` merge the two abstract states using a call-by-value semantics, and
     * remove the common dimension.
     * @todo why not remove the private dimensions before connecting?
     * @todo we should better understand the concrete semantic of this operation
     */
    def connect(other: Property, common: Int): Property
  }
}
