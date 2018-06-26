/**
  * Copyright 2018 Filippo Sestini, Tobia Tesan
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

package it.unich.jandom.utils.numberext

trait IField[N] {
  def +(that : N) : N
  def -(that : N) : N
  def min(that : N) : N
  def max(that : N) : N
  def isPosInfinity : Boolean
  def isNegInfinity : Boolean
  def compare(that: N) : Int
  def >(that: N) : Boolean
  def >=(that: N) : Boolean
  def <(that: N): Boolean
  def <=(that: N): Boolean
  // def !=(that: N): Boolean
  def /(that: N): N
  def unary_- : N
  def _x_2 : N
  def _div_2 : N
}

trait StaticIField[F <: IField[F]] {
  def zero : F
  def PositiveInfinity : F
  def NegativeInfinity : F
}
