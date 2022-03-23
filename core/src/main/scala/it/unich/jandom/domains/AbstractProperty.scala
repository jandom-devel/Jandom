/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.domains

/**
 * The base class for all abstract properties, i.e. elements of abstract domains.
 *
 * Abstract properties theoretically corresponds to concrete one by means of a concretization map
 * \gamma. Two abstract properties are equal when the corresponding concretizations are equal.
 * Since may be difficult to compare, we require abstract equality to only respect the property
 * "a1 == a2 implies \gamma(a1) == \gamma(a2)" (definite equality). As always, "a1 != a2 iff ! (a1 == a2)".
 * Note that a completely different choice would be "a1 != a2 implies \gamma(a1) \cap \gamma(a2) = \emptyset",
 * which is "definite disequality".
 *
 * We assume concrete elements are partially ordered, which induces a partial order on abstract
 * properties: "a1 <= a2 iff \gamma(a1) <= \gamma(a2)". Since computing "a1 <= a2" may not
 * be easy, we only approximatethe real induced ordering. Therefore, we request that
 * "a1 <= a2 implies \gamma(a1) <= \gamma(a2)" (definite inequality). The same for a1 >= a2. This
 * means that the `tryCompareTo` returns `None` each time it cannot (or does not want)
 * determine the relationship between two abstract properties. Finally ==, <= >=, <, > are
 * related by the usual pre-order equations.
 *
 * Abstract properties use F-bounded polymorhpism to ensure type safety,
 * hence a concrete class `C` implementing an abstract property should inherit from
 * `AbstractProperty[C]`.
 *
 * @tparam Property the real class we are endowing with the AbstractProperty quality.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait AbstractProperty[Property <: AbstractProperty[Property]] extends PartiallyOrdered[Property] {
  /**
   * The class of abstract domains which contains this property.
   */
  type Domain <: AbstractDomain

  /**
   * Returns the abstract domain corresponding to this property.
   */
  def domain: Domain

  /**
   * Computes an upper bound of two abstract properties. If it is possible and convenient, this should compute
   * the least upper bound, but it is not a requirement.
   * @param that the abstract object to join with `this`.
   * @note $NOTEFIBER
   * @return an upper bound of the two abstract properties.
   */
  def union(that: Property): Property

  /**
   * Computes an upper approximation of the greatest lower bound of two abstract properties.
   * @param that the abstract object to meet with `this`.
   * @note $NOTEFIBER
   * @return a lower bound of the two abstract properties.
   */
  def intersection(that: Property): Property

  /**
   * The standard widening for two abstract properties. The object `this` should be widened with `that`. The
   * result should be an upper bound of the two properties, and convergemce of the sequence s(i+1) = s(i) widening v(i)
   * should be ensured for any sequence of v(i)'s.
   *
   * Important: `that` is not required to be bigger than `this`. This generality is needed in order to deal with
   * non-monotonic domains or initial assignments which are not pre-fixpoints of the equation system. An alternative
   * could be requiring `that` to be always bigger than `this` but unconditionally applying union before widening. However,
   * this would preclude the possibily to apply heuristics for widening in non-monotonic domains.
   * @param that the abstract object to be widened with `this`.
   * @return the widening of the two abstract properties.
   */
  def widening(that: Property): Property

  /**
   * The standard narrowing for two abstract properties. The object `this` should be narrowed with `that`. The
   * result should be smaller than `this` but bigger than `this intersection that` (or, more generally, a correct
   * approximation of the concrete intersection). Moreover, convergence of the sequence s(i+1) = s(i) narrowing v(i)
   * should be ensured for any sequence of v(i)'s.
   *
   * Important: `that` is not required to be bigger than `this`. This generality is needed in order to deal with
   * non-monotonic domains. When the domain is monotonic, `that` is smaller than `this`, we fall in the standard
   * notion of narrowing we found in the literature.
   * @param that the abstract object to be narrowed with `this`.
   * @return the narrowing of the two abstract properties.
   */
  def narrowing(that: Property): Property

  /**
   * Returns true ONLY IF this an empty element, i.e. it represents un-reachability.
   */
  def isEmpty: Boolean

  /**
   * Determines if two properties are the same on the base of `tryCompareTo`
   */
  override def equals(other: Any): Boolean = other match {
    case other: AbstractProperty[Property @unchecked] => this.tryCompareTo(other) == Some(0)
    case _ => false
  }
}
