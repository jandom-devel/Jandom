/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.objectmodels

/**
 * An ObjectModel encapsulate all the informations on the object model of a language.
 * It also encapsulated differences on the low level libraries used to interpret
 * programs (such as Soot vs ASM).
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectModel {

  /**
   * This is the abstract type for types in target language. A type may be concrete/abstract, primitive/non-primitive.
   * A concrete type may be instantiated, while an abstract one cannot. A primitive types is memorized in the stack
   * or local area of a method, a non-primitive is memorized on the heap.
   */
  type Type

  /**
   * This is the abstract type for fields in the target language.
   */
  type Field

  /**
   * It returns the type of the field `f`.
   */
  def typeOf(f: Field): Type

  /**
   * It returns the set of all the fields declared by type `t` (not by its super- and sub-types). The result
   * of `fieldsOf` for different types should be disjoint.
   */
  def declaredFields(t: Type): Set[Field]

  /**
   * Returns the set of all fields of a given type `t`, both declared and inherited.
   */
  def fields(t: Type): Set[Field]

  /**
   * Returns the set of all fields which an object of declared type `t` must necessarily have.
   */
  def neededFields(t: Type): Set[Field]

  /**
   * Returns the set of all fields which an object of declared type `t` might possibly have.
   */
  def possibleFields(t: Type): Set[Field]

  /**
   * Returns whether a type `t` is primitive. A primitive object is not memorized on the heap,
   * hence cannot be aliased.
   */
  def isPrimitive(t: Type): Boolean

  /**
   * Returns whether a type `t` is concrete. A concrete object may exists somewhere
   * in memory, otherwise it is abstract and cannot be instantiated.
   */
  def isConcrete(t: Type): Boolean

  /**
   * Returns whether some subtype of t is concrete.
   */
  def isConcretizable(t: Type): Boolean

  /**
   * Returns whether a type `t` is an array. Array types are particular types which may be indexed with
   * natural numbers and return elements of a given type. No other assumption is made.
   */
  def isArray(t: Type): Boolean

  /**
   * Returns the element type of the array `t`, or `None` if `t` is not an array type.
   */
  def elementType(t: Type): Option[Type]

  /**
   * Returns the parents of a given type
   */
  def parents(t: Type): Set[Type]

  /**
   * Returns the children of a given type
   */
  def children(t: Type): Set[Type]

  /**
   * Returns the ancestors of type `t`
   */
  def ancestors(t: Type): Set[Type]

  /**
   * Returns the descendants of type `t`
   */
  def descendants(t: Type): Set[Type]

  /**
   * Returns an upper crown for a collection of types. An upper crown of `ts` is a set `crown` subset of
   * `ts` such that each element in `ts` has an upper bound in `crown`.
   */
  def upperCrown(ts: Iterable[Type]): Set[Type]

  /**
   * It returns true iff `t1` is a subtype of `t2`, i.e., if each variable of type `t1`
   * may be saved in a variable of type `t2` without conversion.
   */
  def lteq(t1: Type, t2: Type): Boolean

  /**
   * Returns an upper approximation of all the concrete types which are subtypes of both
   * `t1` and `t2`. When the result is `None`, then `t1` and `t2` have no common concrete
   * subtype.
   */
  def concreteApprox(t1: Type, t2: Type): Option[Type]

  /**
   * Returns an upper approximation of all the concrete types which are subtypes of all
   * the elements of `ts`. When the result is `None`, the types in `ts` have no common concrete
   * subtype.
   */
  def concreteApprox(ts: Iterable[Type]): Option[Type]

  /**
   * Returns an upper approximation of all the concrete subtypes of type `t`. When the result is `None`,
   * then `t` has no concrete subtype.
   */
  def concreteApprox(t: Type): Option[Type]

  /**
   * Determines whether the sequence of fields `fs`, starting from an object of
   * type `t`, may be typed correctly.
   */
  def pathExists(t: Type, fs: Field*): Boolean

  /**
   * Determines an upper crown of the types reachable from a variable of declared type `t`.
   * A type `t2` is reachable from `t` if there is a path that start from an object of declared type `t` and ends
   * in an object of declared type `t2`.
   */
  def reachablesFrom(t: Type): Set[Type]

  /**
   * Determines whether the type `tgt` is reachable from `src`.
   */
  def isReachable(src: Type, tgt: Type): Boolean

  /**
   * Returns true if two variables of type `t1` and `t2` may be aliases. Two variable are
   * aliases when they point to the same location in the heap.
   */
  def mayBeAliases(t1: Type, t2: Type): Boolean

  /**
   * This returns true iff two variables of type t1 and t2 may share. Two variables share
   * if it is possible to reach the same object by following two chains of fields.
   */
  def mayShare(t1: Type, t2: Type): Boolean

}
