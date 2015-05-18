/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils

/**
 * This is an implementation of a type (`TList`) which is list of types. `TNil` is the empty list, while `H::T`
 * is the concatenation of the type `H` with the list `T`.
 */
object TLists {
  
  /**
   * The top of the hierarchy. Every subclass of TList is a list of types. 
   */
  sealed trait TList

  /**
   * The empty list of types.
   */
  sealed trait TNil extends TList  
  
  /**
   * The implementation for the list of types obtained by concatenating `H` and `T`. 
   */
  private [TLists] sealed trait TCons[H, T <: TList] extends TList

  
  /**
   * The list of types obtained by concatenating `H` and `T`. 
   */
  type ::[H, T <: TList] = TCons[H, T]

  /**
   * Shorthand to return a `null` value of type `T`
   */
  private def value[T] = null.asInstanceOf[T]
  
  /**
   * An object of class `Contains[H,T]` is an evidence that the type `H` is an element of the list `T`.
   */
  final class Contains[H, T <: TList]()  
  implicit def contains[H, T <: TList] = value[Contains[H, H :: T]]
  implicit def contains2[H, H2, T <: TList](implicit ev1: Contains[H, T]) = value[Contains[H, H2 :: T]]

  /**
   * An object of class `SubSet[S,T]` is an evidence that the list `S` is a subset of the list `T`.
   */
  final class SubSet[S <: TList, T <: TList]()
  implicit def subset[T <: TList] = value[SubSet[TNil, T]]
  implicit def subset2[H, S <: TList, T <: TList](implicit ev1: Contains[H, T], ev2: SubSet[S,T]) = value[SubSet[H :: S, T]]
}
