/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This filteq is part of JANDOM: JVM-based Analyzer for Numerical DOMains
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

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1

/**
 * This is a trait for test suites of object models.
 * @author Gianluca Amato <gamato@unich.it>
 * @todo Add a generator for paths
 */
trait ObjectModelSuite extends FunSpec with  TableDrivenPropertyChecks  {

  /**
   * The object model we want to test
   */
  val om: ObjectModel

  import om._

  /**
   * All types we want to check
   */
  val someTypes: TableFor1[Type]

  describe("The declared fields of a type") {
    they("are disjoint for different types") {
      for (t1 <- someTypes; t2 <- someTypes; if t1 != t2)
        assert((declaredFields(t1) intersect declaredFields(t2)).isEmpty)
    }
  }

  describe("The typeOf method") {
    it("returns a type for each field") {
      for (t <- someTypes) {
        val fields = declaredFields(t)
        // I want to check absence of exception, I do not know if there is a standard method
        fields map { typeOf(_) }
      }
    }
  }

  describe("The subtype relation") {
    it("is reflexive") {
      for (t <- someTypes)
        assert(lteq(t, t))
    }
    it("is anti-symmetric") {
      for (t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2) && lteq(t2, t1))
        assert(t1 === t2)
    }
    it("is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if lteq(t1, t2) && lteq(t2, t3))
        assert(lteq(t2, t3))
    }
  }

  describe("The parents of a type t") {
    they("are super-types of t") {
      for (t1 <- someTypes; t2 <- parents(t1))
        assert(lteq(t1, t2))
    }
    they("do not contain t itself") {
      for (t <- someTypes)
        assert(!(parents(t) contains t))
    }
  }

  describe("The children of a type t") {
    they("are sub-types of t") {
      for (t1 <- someTypes; t2 <- children(t1))
        assert(lteq(t2, t1))
    }
    they("do not contain t itself") {
      for (t <- someTypes)
        assert(!(children(t) contains t))
    }
  }

  describe("The ancestors of a type t") {
    they("are super-types of t") {
      for (t1 <- someTypes; t2 <- ancestors(t1))
        assert(lteq(t1, t2))
    }
    they("do contain t itself") {
      for (t <- someTypes)
        assert(ancestors(t) contains t)
    }
    they("are a superset of the parents of t") {
      for (t <- someTypes)
        assert(parents(t) subsetOf ancestors(t))
    }
    they("are anti-monotonic w.r.t. sub-type relationship") {
      for (t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2))
        assert(ancestors(t2) subsetOf ancestors(t1))
    }
  }

  describe("The descendants of a type t") {
    they("are sub-types of t") {
      for (t1 <- someTypes; t2 <- descendants(t1))
        assert(lteq(t2, t1))
    }
    they("do contain t itself") {
      for (t <- someTypes)
        assert(descendants(t) contains t)
    }
    they("are a superset of the children of t") {
      for (t <- someTypes)
        assert(children(t) subsetOf descendants(t))
    }
    they("are monotonic w.r.t. sub-type relationship") {
      for (t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2))
        assert(descendants(t1) subsetOf descendants(t2), s"descendants of ${t1} is not a subset of descendants of ${t2}")
    }
  }

  describe("The fields of a type") {
    they("are monotonic w.r.t. subtype relationship") {
      for (t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2))
        assert(fields(t2) subsetOf fields(t1))
    }
    they("contain the decleated fields of the same type") {
      for (t <- someTypes) assert(declaredFields(t) subsetOf fields(t))
    }
  }

  describe("The concretizability of a type") {
    it("is upward closed") {
      for { t1 <- someTypes; t2 <- someTypes; if isConcretizable(t1) && lteq(t1, t2) }
        assert(isConcretizable(t2), s"${t1} may be instantiated but ${t2} cannot")
    }
    it("is always true for concrete types") {
      for (t <- someTypes; if isConcrete(t)) assert(isConcretizable(t))
    }
  }

  describe("The needed fields of a type") {
    they("are empty on non concretizable types") {
      for { t <- someTypes; if !isConcretizable(t) } {
        assert(neededFields(t).isEmpty)
      }
    }
    they("contain the fields of the type if the type is concretizable") {
      for { t <- someTypes; if isConcretizable(t) } {
        assert(fields(t) subsetOf neededFields(t))
      }
    }
    they("are anti-monotone w.r.t. subtype relationship for concretizable types") {
      for { t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2) && isConcretizable(t1) } {
        val fields1 = neededFields(t1)
        val fields2 = neededFields(t2)
        assert(fields2 subsetOf fields1, s"${fields2} is not a subset of ${fields1}")
      }
    }
  }

  describe("The possible fields of a type") {
    they("are empty on non concretizable types") {
      for { t <- someTypes; if !isConcretizable(t) } {
        assert(possibleFields(t).isEmpty)
      }
    }
    they("are a superset of the needed fields") {
      for { t <- someTypes } {
        assert(neededFields(t) subsetOf possibleFields(t))
      }
    }
    they("are monotone w.r.t. subtype relationship") {
      for { t1 <- someTypes; t2 <- someTypes; if lteq(t1, t2) } {
        val fields1 = possibleFields(t1)
        val fields2 = possibleFields(t2)
        assert(fields1 subsetOf fields2, s"${fields1} is not a subset of ${fields2}")
      }
    }
  }

  describe("Type reachability") {
    it("is reflexive for types which may share with other types") {
      for { t1 <- someTypes; t2 <- someTypes; if mayShare(t1, t2) }
        assert(reachablesFrom(t1) contains t1)
    }
    it("is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (isReachable(t1, t2) && isReachable(t2, t3)) {
          assert(isReachable(t1, t3), s"${t1} may reach ${t2} which may reach ${t3}, but ${t1} cannot reach ${t3}")
        }
      }
    }
    it("is downward closed on the target") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (isReachable(t1, t2) && lteq(t3, t2)) {
          assert(isReachable(t1, t3))
        }
      }
    }
    it("is upward closed on the source") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (isReachable(t1, t2) && lteq(t1, t3)) {
          assert(isReachable(t3, t2))
        }
      }
    }
  }

  describe("The upper crown operator") {
    it("returns empty set for empty sequences") {
      assert(upperCrown(Seq()).isEmpty)
    }
    it("returns the only element in length one sequence") {
      for (t <- someTypes)
        assert(upperCrown(Seq(t)) == Set(t))
    }
    it("is commutative") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        val base = upperCrown(Seq(t1, t2, t3))
        assert(upperCrown(Seq(t1, t3, t2)) === base)
        assert(upperCrown(Seq(t2, t1, t3)) === base)
        assert(upperCrown(Seq(t2, t3, t1)) === base)
        assert(upperCrown(Seq(t3, t1, t2)) === base)
        assert(upperCrown(Seq(t3, t2, t1)) === base)
      }
    }
    it("returns a set of incomparable elements") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        val crown = upperCrown(Seq(t1, t2, t3))
        for (ta <- crown; tb <- crown; if ta != tb)
          assert(!lteq(ta, tb) && !lteq(tb, ta), s"ta = ${ta} and tb = ${tb}")
      }
    }
    it("is equal to the concreteApprox when the two arguments are equal") {
      for (t <- someTypes) {
        assert(concreteApprox(t) === concreteApprox(t, t), s"for t = ${t}")
      }
    }
  }

  describe("The binary concreteApprox operator") {
    it("is idempotent on concrete type, reductive on concretizable types, empty on non concretizables") {
      for (t <- someTypes) {
        val glb = concreteApprox(t, t)
        if (isConcrete(t)) {
          assert(glb.isDefined)
          assert(glb.get === t)
        } else if (isConcretizable(t)) {
          assert(glb.isDefined)
          assert(lteq(glb.get, t))
        } else {
          assert(glb.isEmpty)
        }
      }
    }
    it("is bigger than all other concrete lower bounds") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if lteq(t3, t1) && lteq(t3, t2) && isConcrete(t3)) {
        val glb = concreteApprox(t1, t2)
        assert(glb.isDefined)
        assert(lteq(t3, glb.get))
      }
    }
    it("returns a concretizable type") {
      for (t1 <- someTypes; t2 <- someTypes; glb = concreteApprox(t1, t2); if glb.isDefined) {
        assert(isConcretizable(glb.get))
      }
    }
  }

  describe("The elementType method") {
    it("returns a type for arrays") {
      for (t <- someTypes; if (isArray(t)))
        assert(elementType(t).isDefined)
    }
  }

  describe("The pathExists method") {
    it("returns true for empty paths") {
      for (t <- someTypes)
        assert(pathExists(t))
    }
    it("returns true for  qualified identifiers") {
      for (t <- someTypes; f <- fields(t))
        assert(pathExists(t,f))
    }
     it("returns false for non-valid qualified identifiers") {
      for (t <- someTypes; t2 <- someTypes; if !lteq(t,t2);  f <- declaredFields(t2))
        assert(!pathExists(t,f))
    }
  }

  describe("The concreteApprox of a sequence of types") {
    it("is undefined for empty sequences") {
      assert(concreteApprox(Seq()).isEmpty)
    }
    it("is equivalent to binary concreteApprox with two argument equals for length one sequences") {
      for (t <- someTypes) {
        assert(concreteApprox(Seq(t)) === concreteApprox(t, t))
      }
    }
    it("is equal to the binary concreteApprox for length two sequences") {
      for (t1 <- someTypes; t2 <- someTypes) {
        assert(concreteApprox(Seq(t1, t2)) === concreteApprox(t1, t2))
      }
    }
    it("may be obtained by iterating the binary glb operator") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        val ts = Seq(t1, t2, t3)
        assert(concreteApprox(ts) === (concreteApprox(t2, t3) flatMap { concreteApprox(t1, _) }), s"for glb of ${t1}, ${t2} and ${t3}")
      }
    }
  }

  describe("Possible aliasing information") {
    it("is reflexive on non-primitive concretizable types") {
      for (t <- someTypes; if !isPrimitive(t) && isConcretizable(t))
        assert(mayBeAliases(t, t))
    }
    it("is symmetric") {
      for (t1 <- someTypes; t2 <- someTypes)
        assert(mayBeAliases(t1, t2) === mayBeAliases(t2, t1))
    }
    it("corresponds to having a defined concreteApprox and being non-primitive") {
      for (t1 <- someTypes; t2 <- someTypes) {
        val aliasable = concreteApprox(t1, t2).isDefined && !isPrimitive(t1) && !isPrimitive(t2)
        assert(mayBeAliases(t1, t2) === aliasable)
      }
    }
    it("is upward closed") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if mayBeAliases(t1, t2)) {
        if (lteq(t2, t3)) assert(mayBeAliases(t1, t3), s"${t1} may be alias with ${t2} and ${t1} <= ${t3}")
        if (lteq(t1, t3)) assert(mayBeAliases(t2, t3), s"${t1} may be alias with ${t2} and ${t2} <= ${t3}")
      }
    }
  }

  describe("Possible sharing information") {
    it("is reflexive for non-primitive concretizable types") {
      for { t <- someTypes; if !isPrimitive(t) && isConcretizable(t) }
        assert(mayShare(t, t))
    }
    it("is reflexive for types which shares with other types") {
      for (t1 <- someTypes; t2 <- someTypes; if mayShare(t1, t2)) {
        assert(mayShare(t1, t1))
      }
    }
    it("is symmetric") {
      for { t1 <- someTypes; t2 <- someTypes }
        assert(mayShare(t2, t1) === mayShare(t2, t1))
    }
    it("is upward closed") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes; if mayShare(t1, t2)) {
        if (lteq(t2, t3)) assert(mayShare(t1, t3), s"${t1} share with ${t2} and ${t1} <= ${t3}")
        if (lteq(t1, t3)) assert(mayShare(t2, t3), s"${t1} share with ${t2} and ${t2} <= ${t3}")
      }
    }
  }
}
