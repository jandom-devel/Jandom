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

package it.unich.jandom.targets

import org.scalatest._
import org.scalacheck._

import it.unich.jandom.targets.jvmsoot.SootObjectModel
import it.unich.jandom.objectmodels.ObjectModelSuite

import soot._

/**
 * Test class for SootObjectModel
 * @author Gianluca Amato
 */
class SootObjectModelSuite extends FunSpec with ObjectModelSuite with SootTests {

  val scene = initSoot("java")
  val klassA = RefType.v(scene.loadClassAndSupport("javatest.A"))
  val klassB = RefType.v(scene.loadClassAndSupport("javatest.B"))
  val klassListA = RefType.v(scene.loadClassAndSupport("javatest.ListA"))
  val klassListA2 = RefType.v(scene.loadClassAndSupport("javatest.ListA2"))
  val klassListA3 = RefType.v(scene.loadClassAndSupport("javatest.ListA3"))
  val interfaceList = RefType.v(scene.loadClassAndSupport("javatest.ListInterface"))
  val interfaceList2 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface2"))
  val interfaceList3 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface3"))
  val interfaceOther = RefType.v(scene.loadClassAndSupport("javatest.OtherInterface"))
  val interfaceInstantiable = RefType.v(scene.loadClassAndSupport("javatest.InstantiableInterface"))
  val klassAbs = RefType.v(scene.loadClassAndSupport("javatest.Abs"))
  val klassAbs1 = RefType.v(scene.loadClassAndSupport("javatest.Abs1"))
  val klassNoAbs1 = RefType.v(scene.loadClassAndSupport("javatest.NoAbs1"))
  val klassAbs2 = RefType.v(scene.loadClassAndSupport("javatest.Abs2"))
  val klassPair = RefType.v(scene.loadClassAndSupport("javatest.Pair"))
  val klassS1 = RefType.v(scene.loadClassAndSupport("javatest.S1"))
  val klassS2 = RefType.v(scene.loadClassAndSupport("javatest.S2"))
  val klassS3 = RefType.v(scene.loadClassAndSupport("javatest.S3"))
  val klassS4 = RefType.v(scene.loadClassAndSupport("javatest.S4"))
  val klassS5 = RefType.v(scene.loadClassAndSupport("javatest.S5"))
  val klassR3 = RefType.v(scene.loadClassAndSupport("javatest.R3"))
  val klassK = RefType.v(scene.loadClassAndSupport("javatest.K"))
  val primitiveInt = IntType.v()
  val primitiveByte = ByteType.v()
  val arrPrimitive = ArrayType.v(primitiveInt, 2)
  val arrS3dim2 = ArrayType.v(klassS3, 2)
  val arrS3dim1 = ArrayType.v(klassS3, 1)
  val arrS1dim1 = ArrayType.v(klassS1, 1)
  val arrIface = ArrayType.v(interfaceList, 2)
  val arrIface2 = ArrayType.v(interfaceList2, 2)

  val someTypes = Table("type", klassA, klassB, klassListA, klassPair, klassS1, klassS2, klassS3, klassS4,
    klassS5, klassR3, interfaceList, interfaceList, interfaceList2, interfaceList3, interfaceOther,
    interfaceInstantiable, klassAbs, klassAbs1, klassNoAbs1, klassAbs2, klassK,
    primitiveInt, primitiveByte, arrPrimitive, arrS3dim2, arrS3dim1, arrS1dim1, arrIface, arrIface2,
    klassListA2, klassListA3)

  val om = new SootObjectModel(scene)

  import om._

  describe("The subtype relation") {
    it("passes some specific test for SootObjectModel") {
      assert(lteq(klassS3, klassS1))
      assert(lteq(klassS2, klassS1))
      assert(lteq(klassS3, klassS2))
      assert(lteq(klassListA, interfaceList))
      assert(lteq(interfaceList2, interfaceList))
      assert(lteq(primitiveInt, primitiveInt))
      assert(!lteq(primitiveByte, primitiveInt))
    }
    it("is covariant on arrays") {
      assert(lteq(arrS3dim1, arrS1dim1))
    }
  }

  describe("The concreteApprox operation") {
    it("passes some specific test for SootObjectModel") {
      assert(concreteApprox(interfaceList3, klassListA).isDefined)
      assert(concreteApprox(interfaceList, interfaceOther) === Some(klassListA3 ))
      assert(concreteApprox(Seq(interfaceList, interfaceOther, klassListA)) === Some(klassListA3))
      assert(concreteApprox(Seq(interfaceList, klassA, interfaceOther, klassListA)) === None)
    }
  }

  describe("The set of fields of a type") {
    it("is empty on primitive types") {
      for { t <- someTypes; if isPrimitive(t) }
        assert(fields(t).isEmpty)
    }
    it("is empty on interfaces") {
      for { t <- someTypes; if isInterface(t) }
        assert(fields(t).isEmpty)
    }
    it("passes some specific test for SootObjectModel") {
      val f1 = klassS2.getSootClass().getField("f1", klassA)
      val f2 = klassS3.getSootClass().getField("f2", klassB)
      assert(fields(klassS3) === Set(f1,f2))
    }
  }

  describe("The possible fields of a class/interface") {
    it("is empty on primitive types") {
      for { t <- someTypes; if isPrimitive(t) } {
        assert(possibleFields(t).isEmpty)
      }
    }
    it("passes some specific test for SootObjectModel") {
      val f1 = klassS2.getSootClass().getField("f1", klassA)
      val f2 = klassS3.getSootClass().getField("f2", klassB)
      val l = klassS4.getSootClass().getField("l", klassListA)
      assert(possibleFields(klassS3) === Set(f1, f2))
      assert(possibleFields(klassS2) === Set(f1, f2, l))
    }
  }

  describe("The instantiability of a type") {
    it("passes some specific tests for SootObjectModel") {
      assert(isConcretizable(interfaceList))
      assert(!isConcretizable(interfaceList2))
      assert(isConcretizable(interfaceList3))
      assert(isConcretizable(klassPair))
      assert(isConcretizable(arrIface))
      assert(isConcretizable(arrIface2))
    }
  }

  describe("Type reachability") {
    it("passes some specific test for SootObjectModel") {
      assert(!isReachable(interfaceList2, interfaceList2))
      assert(isReachable(klassA, klassA))
      assert(!isReachable(klassA, klassB))
      assert(isReachable(klassListA, klassA))
      assert(isReachable(klassR3, klassS5))
      assert(!isReachable(klassR3, klassS4))
      assert(isReachable(klassR3, klassS3))
      assert(!isReachable(klassR3, klassS2))
    }
  }

  describe("Type sharing") {
    it("passes some specific test for SootObjectModel") {
      assert(mayShare(klassA, klassA))
      assert(!mayShare(klassA, klassB))
      assert(mayShare(klassA, klassListA))
      assert(!mayShare(klassB, klassListA))
      assert(mayShare(klassA, klassPair))
      assert(mayShare(klassListA, klassPair))
      assert(mayShare(klassB, klassPair))
      assert(mayShare(interfaceList, interfaceOther))
    }
  }
}
