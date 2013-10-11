/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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

package it.unich.sci.jandom.targets.jvmsoot

import scala.annotation.elidable
import scala.annotation.elidable._
import scala.collection.immutable.Stack

import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.LinearForm
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.linearcondition.LinearCond

import soot._
import soot.baf.WordType
import soot.baf.DoubleWordType
import soot.jimple.Constant
import soot.jimple.StaticFieldRef

/**
 * This class implements an abstract frame for Soot where only numerical variables are considered.
 * @param numdom the numerical domain to use for representing properties of numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
 */
class SootFrameNumericalDomain(val numdom: NumericalDomain) extends SootFrameDomain {

  def top(vars: Seq[Type]) = Property(numdom.top(vars.size), Stack(vars.reverse: _*))

  def bottom(vars: Seq[Type]) = Property(numdom.bottom(vars.size), Stack(vars.reverse: _*))

  /**
   * A simple helper method for the analogous constructor of abstract numerical frames.
   */
  def apply(prop: numdom.Property, types: Seq[Type]) = new Property(prop, Stack(types.reverse: _*))

  /**
   * A simple helper method for the analogous constructor of abstract numerical frames.
   */
  def apply(prop: numdom.Property, tpe: Type) = new Property(prop, tpe)

  /*
    private def canonicalType(tpe: Type) = tpe match {
      case _ : BooleanType => IntType.v()
      case _ : ByteType => IntType.v()
      case _ : ShortType => IntType.v()
      case x  => x
    }
  */

  /**
   * This class represents a single abstract frame.
   * @param prop contains the numeric property giving informations on numerical variables. Dimensions are allocated also for
   * non-numerical variables, but their value is undetermined
   * @param vars the stack of variable types in the current frame. Note that stack position are numbered in the
   * opposite way than frame variables, i.e., the frame variable `i` corresponds to stack position `size - 1 - i`.
   */
  case class Property(val prop: numdom.Property, val fiber: Stack[Type]) extends SootFrameProperty[Property] {

    invariantCheck

    type Domain = SootFrameNumericalDomain.this.type

    def domain = SootFrameNumericalDomain.this

    def dimension = fiber.length

    /**
     * An alternative constructor which returns an abstract frame where all variables are of the same type.
     * @param prop the numerical property to use.
     * @param tpe the common type of all frame variables.
     */
    def this(prop: numdom.Property, tpe: Type) = {
      this(prop, Stack((for (i <- 0 until prop.dimension) yield tpe): _*))
    }

    /**
     * This method check invariants on a numerical abstract frame.
     */
    @elidable(ASSERTION)
    private def invariantCheck {
      assert(prop.dimension == fiber.size, "Numerical property and stack of types have different dimensions")
      if (prop.isEmpty) return
      for (i <- 0 until prop.dimension) fiber(size - 1 - i) match {
        case _: PrimType | _: WordType | _: DoubleWordType =>
        case _ =>
          // TODO: I should check that dimension i is unconstrained. For now I check that it is unbounded
          assert(prop.minimize(LinearForm.v(i)).isNegInfinity, "A non-numerical variable should be unconstrained")
          assert(prop.maximize(LinearForm.v(i)).isPosInfinity, "A non-numerical variable should be unconstrained")
      }
    }

    def size = prop.dimension

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property =>
          assume(other.fiber == fiber,"The abstract frame have different variables")
          prop tryCompareTo other.prop
        case _ => None
      }

    /**
     * Add a new undetermined frame variable.
     * @param tpe the type of the new frame variable.
     */
    def addVariable(tpe: Type) = Property(prop.addVariable(), fiber.push(tpe))

    def delVariable(m: Int) = ???

    def mapVariables(rho: Seq[Int]) = ???
    /**
     * Remove the top frame variable.
     */
    private def delVariable = Property(prop.delVariable(), fiber.pop)

 	def evalConstant(const: Int) = Property(prop.addVariable().constantAssignment(size, const), fiber.push(IntType.v()))
 	def evalConstant(const: Double) = Property(prop.addVariable().constantAssignment(size, const), fiber.push(DoubleType.v()))
    def evalConstant(const: String) = Property(prop.addVariable().nonDeterministicAssignment(size), fiber.push(RefType.v(const.getClass().getName())))

    def evalNull = addVariable(NullType.v())

    def evalNew(tpe: Type) = addVariable(tpe)

    def evalLocal(v: Int) = {
      if (fiber(size - 1 - v).isInstanceOf[PrimType] || fiber(size - 1 - v).isInstanceOf[WordType] || fiber(size - 1 - v).isInstanceOf[DoubleWordType])
        Property(prop.addVariable().variableAssignment(size, v), fiber.push(fiber(size - 1 - v)))
      else
        addVariable(fiber(size - 1 - v))
    }

    def evalField(v: Int, f: SootField) = {
      assume(fiber(size - 1 - v).isInstanceOf[RefType],"Expected RefType, got "+fiber(size - 1 - v))
      addVariable(f.getType())
    }

    def assignLocal(dst: Int, src: Int) = {
      // TODO: I would like to put an assume, but I am not sure what to check
      Property(prop.variableAssignment(dst, src), fiber)
    }

    def assignLocal(dst: Int) = {
      if (fiber(size - 1 - dst).isInstanceOf[PrimType] || fiber(size - 1 - dst).isInstanceOf[WordType] || fiber(size - 1 - dst).isInstanceOf[DoubleWordType]) {
        Property(prop.variableAssignment(dst, size - 1).delVariable(), fiber.pop)
      } else
        delVariable
    }

    def assignField(dst: Int, f: SootField) = {
      assume(fiber(size - 1 - dst).isInstanceOf[RefType],"Expected RefType, got "+fiber(size - 1 - dst))
      delVariable
    }

    def assignStaticField(dst: Int, f: SootField) = {
      delVariable
    }

    def evalInstance (t: Type) = {
      prop.nonDeterministicAssignment(size-1)
      delVariable
    }

    def evalAdd = Property(prop.variableAdd().delVariable(), fiber.pop)
    def evalSub = Property(prop.variableSub().delVariable(), fiber.pop)
    def evalMul = Property(prop.variableMul().delVariable(), fiber.pop)
    def evalDiv = Property(prop.variableDiv().delVariable(), fiber.pop)
    def evalRem = Property(prop.variableRem().delVariable(), fiber.pop)
    def evalShl = Property(prop.variableShl().delVariable(), fiber.pop)
    def evalShr = Property(prop.variableShr().delVariable(), fiber.pop)
    def evalUshr = Property(prop.variableUshr().delVariable(), fiber.pop)

    def evalBinOp = Property(prop.delVariable().delVariable().addVariable(), fiber.pop)
    def evalNeg = Property(prop.variableNeg(), fiber)
    def evalLength = addVariable(IntType.v())

    def evalGt = delVariable
    def evalGe = delVariable
    def evalLt = delVariable
    def evalLe = delVariable
    def evalEq = delVariable
    def evalNe = delVariable

    /**
     * A generic test operation which depends on a comparison operator.
     * @param op the comparison operator to use
     */
    private def testComp(op: AtomicCond.ComparisonOperators.Value) = {
      import AtomicCond.ComparisonOperators._
      val lf = LinearForm(0, size-2 -> 1, size-1 -> -1)
      val tbranch = Property(AtomicCond(lf, op).analyze(prop).delVariable().delVariable(), fiber.pop.pop)
      val fbranch = Property(AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op)).analyze(prop).delVariable().delVariable(), fiber.pop.pop)
      (tbranch, fbranch)
    }

    def testGt = testComp(AtomicCond.ComparisonOperators.GT)
    def testGe = testComp(AtomicCond.ComparisonOperators.GTE)
    def testLe = testComp(AtomicCond.ComparisonOperators.LTE)
    def testLt = testComp(AtomicCond.ComparisonOperators.LT)
    def testEq = testComp(AtomicCond.ComparisonOperators.EQ)
    def testNe = testComp(AtomicCond.ComparisonOperators.NEQ)

    def testLinearCondition(lc: LinearCond) = (
      Property(lc.analyze(prop), fiber), Property(lc.opposite.analyze(prop), fiber))

    def union(that: Property) = Property(prop union that.prop, fiber)

    def intersection(that: Property) = Property(prop intersection that.prop, fiber)

    def widening(that: Property) = Property(prop widening that.prop, fiber)

    def narrowing(that: Property) = Property(prop widening that.prop, fiber)

    def extract(n: Int) = {
      assume (n >= 0 && n <= size, s"Trying to extract ${n} variables in the abstract frame {$this}")
      Property( prop.delVariables(0 until size-n), fiber.dropRight(size-n))
    }

    def restrict(n: Int) = {
      assume (n >= 0 && n <= size, s"Trying to restrict ${n} variables top in the abstract frame {$this}")
      Property( prop.delVariables(size-n until size), fiber.drop(n))
    }

    def connect(p: Property, common: Int) = {
      assume ( (fiber.dropRight(size-common) zip p.fiber.drop(p.size - common)) forall { case (tdst, tsrc) => compatibleTypes(tdst, tsrc) })
      Property(prop.connect(p.prop, common), p.fiber.dropRight(common) ++ fiber.drop(common))
    }

    def enterMonitor(n: Int): Property = this

    def exitMonitor(n: Int): Property = this

    def evalGlobal(o: Constant): Property = addVariable(o.getType())

    def evalStaticField(v: SootField): Property = {
      if (v.getType().isInstanceOf[PrimType])
        Property(prop.addVariable().nonDeterministicAssignment(prop.dimension), fiber.push(v.getType()))
      else
        addVariable(v.getType())
    }

    def evalSwap(i: Int, j:Int): Property = {
            val seq = ((0 until i-1) :+ j) ++ ((i+1 until j-1) :+ i) ++ (j+1 until size-1)
            val st1 = fiber.updated(i, fiber.apply(j))
    		val st2 = st1.updated(j, fiber.apply(i))
            Property(prop.mapVariables(seq), st2)
    }

    def top = Property(prop.top, fiber)

    def bottom = Property(prop.bottom, fiber)

    def isTop = prop.isTop

    def isBottom = prop.isBottom

    def isEmpty = prop.isEmpty


    /*  def isCompatibleWith(that: Property) =
    	prop == that.prop &&
    	vars.size == that.vars.size &&
    	(vars map canonicalType) == (that.vars map canonicalType)*/

    def mkString(vars: Seq[String]) = prop.mkString(vars) + "types: " + this.fiber.reverse.mkString(",")
  }
}
