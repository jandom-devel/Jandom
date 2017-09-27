/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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

package it.unich.jandom.targets.jvmsoot

import scala.collection.JavaConverters._

import it.unich.jandom.objectmodels.ObjectModel
import it.unich.jandom.objectmodels.ObjectModelHelper

import soot._
import soot.jandom.MyFastHierarchy

/**
 * An object model for the JVM using the Soot library.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class SootObjectModel(scene: soot.Scene) extends ObjectModel with ObjectModelHelper {


  type Type = soot.Type

  type Field = soot.SootField

  /**
   * Returns a MyFastHierarchy, which is a SootFastHirearchy modified to expose direct subinterface
   * relationship.
   */
  val fh = if (scene.hasFastHierarchy() && scene.getFastHierarchy().isInstanceOf[MyFastHierarchy])
    scene.getFastHierarchy().asInstanceOf[MyFastHierarchy]
  else {
    val newfh = new MyFastHierarchy
    scene.setFastHierarchy(newfh)
    newfh
  }

  def declaredFields(t: Type) = t match {
    case t: RefType => t.getSootClass().getFields().asScala.toSet
    case _: PrimType => Set()
    case _: ArrayType => Set()
    case _: NullType => Set()
  }

  def typeOf(f: Field) = f.getType()

  def isPrimitive(t: Type) = t.isInstanceOf[PrimType]

  def isConcrete(t: Type) = t match {
    case t: RefType => t.getSootClass().isConcrete()
    case _: PrimType => true
    case _: ArrayType => true
    case _: NullType => false
  }

  def isArray(t: Type) = t.isInstanceOf[ArrayType]

  def elementType(t: Type) = t match {
    case t: ArrayType => Some(t.baseType)
    case _ => None
  }

 /**
   * Returns whether a type is an interface.
   */
  def isInterface(t: Type) =
    t.isInstanceOf[RefType] && t.asInstanceOf[RefType].getSootClass().isInterface()

  /**
    * @inheritdoc
    * For the moment, we consider primitive types to be incomparable, but I do not know
    * if it is the correct way to handle this.
    */
  def lteq(t1: Type, t2: Type) = fh.canStoreType(t1, t2)

  def parents(t: Type) = t match {
    case t: RefType =>
      val k = t.getSootClass()
      val ifs: Set[Type] = (for {
        i <- k.getInterfaces().asScala
      } yield i.getType())(collection.breakOut)
      if (k.hasSuperclass())
        ifs + k.getSuperclass().getType()
      else
        ifs
    case _: PrimType => Set()
    case t: ArrayType => parents(t.baseType) map { (ArrayType.v(_, t.numDimensions)) }
    case _: NullType => Set()
  }

  def children(t: Type) = t match {
    case t: RefType =>
      val k = t.getSootClass()
      if (k.isInterface()) {
        (fh.getAllImplementersOfInterface(k).asScala map { _.getType() }).toSet ++
          (fh.getSubinterfaces(k).asScala map { _.getType() }).toSet
      } else {
        (fh.getSubclassesOf(k).asScala map { _.getType() }).toSet
      }
    case _: PrimType => Set()
    case t: ArrayType => children(t.baseType) map { (ArrayType.v(_, t.numDimensions)) }
    case _: NullType => Set()
  }

  /**
   * This is a fast approximation of concreteApprox which do not consider concretizability
   * of a type and makes all interfaces equivalent to the Object type. It should be always
   * a super-type of glbApprox. Moreover, if it is undefined, glbApprox should be undefined too.
   */
  def concreteApproxFast(t1: Type, t2: Type): Option[Type] = {
    val tt1 = if (isInterface(t1))
      scene.getObjectType()
    else t1
    val tt2 = if (isInterface(t2))
      scene.getObjectType()
    else t2
    if (lteq(tt1, tt2))
      Some(tt1)
    else if (lteq(tt2, tt1))
      Some(t2)
    else
      None
  }
}
