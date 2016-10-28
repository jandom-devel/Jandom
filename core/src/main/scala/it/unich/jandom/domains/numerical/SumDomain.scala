/**
 * Copyright 2014, 2016 Gianluca Amato, Francesca Scozzari, Simone Di Nardo Di Maio
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
 * You shosuld have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.numerical

/**
 * This is the trait which implements the sum of two abstract domains.
 * @author Gianluca Amato
 * @author Francesca Scozzari
 * @author Simone Di Nardo Di Maio
 */

abstract class SumDomain[D1 <: NumericalDomain, D2 <: NumericalDomain] extends NumericalDomain {

  val dom1: D1

  val dom2: D2

  type Property <: Sum

  abstract class Sum extends NumericalProperty[Property] {

    this: Property =>

    val p1: dom1.Property

    val p2: dom2.Property

    require(p1.dimension == p2.dimension)

    type Domain = SumDomain.this.type

    def domain = SumDomain.this

    def union(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty)
        that
      else if (that.isEmpty)
        this
      else {
        val q1 = p1 union that.p1
        val q2 = p2 union that.p2
        SumDomain.this(q1, q2)
      }
    }

    def widening(that: Property): Property = {
      require(dimension == that.dimension)
      SumDomain.this(p1 widening that.p1, p2 widening that.p2)
    }

    def narrowing(that: Property): Property = {
      require(dimension == that.dimension)
      SumDomain.this(p1 narrowing that.p1, p2 narrowing that.p2)
    }

    def intersection(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty)
        this
      else if (that.isEmpty)
        that
      else if (that.isTop)
        this
      else
        that
    }

    def nonDeterministicAssignment(n: Int): Property = {
      // we apply forget on the first component, and we revert to the second
      // component only if the first one is top
      val q1 = p1.nonDeterministicAssignment(n)
      if (q1.isTop) {
        val q2 = p2.nonDeterministicAssignment(n)
        SumDomain.this(p1, q2)
      } else
        SumDomain.this(q1, p2)
    }

    def linearAssignment(n: Int, lf: LinearForm): Property = {
      // we divide the known coefficient evenly between the two domains
      // a better choice could be performed by knowing some info on the two domains
      val newlf = new DenseLinearForm(lf.known / 2 +: lf.homcoeffs)
      var q1 = p1.linearAssignment(n, newlf)
      var q2 = p2.linearAssignment(n, newlf)
      SumDomain.this(q1, q2)
    }

    def linearInequality(lf: LinearForm): Property = {
      if (p1.isEmpty || p2.isEmpty)
        this
      else {
        var w1 = p1.minimize(lf)
        var w2 = p2.minimize(lf)
        if (!w1.isInfinity && !w2.isInfinity) {
          val lf_k1 = new DenseLinearForm(w2.value +: lf.homcoeffs)
          val lf_k2 = new DenseLinearForm(w1.value +: lf.homcoeffs)
          SumDomain.this(p1.linearInequality(lf_k1), p2.linearInequality(lf_k2))
        } else if (!w1.isInfinity) {
          val lf_k2 = new DenseLinearForm(w1.value +: lf.homcoeffs)
          SumDomain.this(p1, p2.linearInequality(lf_k2));
        } else if (!w2.isInfinity) {
          val lf_k1 = new DenseLinearForm(w2.value +: lf.homcoeffs)
          SumDomain.this(p1.linearInequality(lf_k1), p2);
        } else
          this
      }
    }

    def linearDisequality(lf: LinearForm): Property = this

    def minimize(lf: LinearForm) = {
      val homlf = lf.hom
      p1.minimize(homlf) + p2.minimize(homlf) + lf.known
    }

    def maximize(lf: LinearForm) = {
      val homlf = lf.hom
      p1.maximize(homlf) + p2.maximize(homlf) + lf.known
    }

    def frequency(lf: LinearForm) = {
      val homlf = lf.hom
      (p1.frequency(homlf), p2.frequency(homlf)) match {
        case (Some(v1), Some(v2)) => Option(v1 + v2 + lf.known)
        case _ => Option.empty
      }
    }

    def constraints = {
      if (isEmpty)
        Seq(LinearForm(1))
      else {
        val cs1 = p1.constraints
        val cs2 = p2.constraints
        val set1 = for (c <- cs1 ++ cs2; max = maximize(c.hom); if !max.isInfinity) yield LinearForm(-max.value +: c.homcoeffs: _*)
        val set2 = for (c <- cs1 ++ cs2; min = minimize(c.hom); if !min.isInfinity) yield LinearForm(min.value +: (-c).homcoeffs: _*)
        set1 ++ set2
      }
    }

    def isPolyhedral = p1.isPolyhedral && p2.isPolyhedral

    def addVariable: Property = SumDomain.this(p1.addVariable, p2.addVariable)

    def delVariable(n: Int): Property = SumDomain.this(p1.delVariable(n), p2.delVariable(n))

    def mapVariables(rho: Seq[Int]): Property = SumDomain.this(p1.mapVariables(rho), p2.mapVariables(rho))

    def dimension: Int = p1.dimension

    def isTop: Boolean = (p1.isTop || p2.isTop) && (!p1.isEmpty && !p2.isEmpty)

    def isEmpty = p1.isEmpty || p2.isEmpty

    def isBottom: Boolean = p1.isBottom && p2.isBottom

    def bottom: Property = SumDomain.this(p1.bottom, p2.bottom)

    def top: Property = SumDomain.this(p1.top, p2.top)

    def mkString(vars: Seq[String]): String = {
      if (isEmpty)
        "empty"
      else
        s"${p1.mkString(vars)} + ${p2.mkString(vars)}"
    }

    def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {
      case that: Sum => {
        if (this.isTop && that.isTop)
          Option(0)
        else if (this.isTop)
          Option(1)
        else if (that.isTop)
          Option(-1)
        else if (this.isBottom && that.isBottom)
          Option(0)
        else if (this.isBottom)
          Option(-1)
        else if (that.isBottom)
          Option(1)
        else if (p1 == that.p1)
          p2.tryCompareTo(that.p2)
        else if (p1 > that.p1)
          if (p2 >= that.p2) Option(1) else Option.empty
        else if (p2 <= that.p2) Option(-1)
        else Option.empty
      }
      case _ => Option.empty
    }
  }

  def top(n: Int) = {
    val dom1Zero = (0 until n).foldLeft(dom1.top(n)) { (p, i) => p.linearAssignment(i, LinearForm.c(0)) }
    SumDomain.this(dom1Zero, dom2.top(n))
  }

  def bottom(n: Int) = SumDomain.this(dom1.bottom(n), dom2.bottom(n))

  def apply(p1: dom1.Property, p2: dom2.Property): Property
}
