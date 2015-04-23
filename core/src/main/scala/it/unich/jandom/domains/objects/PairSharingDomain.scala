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

import scala.collection.immutable.Range
import it.unich.jandom.objectmodels.ObjectModel
import scala.util.parsing.combinator.RegexParsers

/**
 * This is the implementation of the pair sharing domain in [Spoto and Secci].
 * @tparam OM the object model type used to build the domain. It is generally `om.type`.
 * @author Gianluca Amato <gamato@unich.it>
 */
class PairSharingDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  def top(types: Seq[om.Type]) = allPairs(0 until types.size, types)

  def bottom(types: Seq[om.Type]) = apply(Set(), types)

  /**
   * Builds a pair sharing object from a set of pairs and a sequence of types.
   * @param ps a set of unordered pairs, which are the pairs of variable which may possibly share
   * @param types a sequence of types for the variables in ps
   */
  def apply(ps: Set[UP[Int]], types: Seq[om.Type]) = new Property(ps, types.reverse)

  /**
   * Builds a pair sharing object from a string representation. The string has the form
   * "{ ( v1, v2 ), ( v3, v4 ), ... }" where v1, v2, etc.. are variable names.
   * @param s the string representation
   * @param varNames the name of variables
   * @param varTypes the type of variables
   */
  def apply(s: String, varNames: Seq[String], varTypes: Seq[om.Type]): Property = {
    val psParser = new PairSharingDomain.PairSharingParser(varNames)
    this(psParser.parseProperty(s).get.toSet, varTypes)
  }

  /**
   * Build a pair sharing object made of all pairs of variable which may share.
   * @param vars the variables which may freely share between them
   * @param types a sequence of types for the variables in ps
   */
  def allPairs(vars: Seq[Int], types: Seq[om.Type]) = {
    val pairs = for (i <- 0 until vars.size; j <- 0 until vars.size; if om.mayShare(types(i), types(j))) yield UP(vars(i), vars(j))
    apply(pairs.toSet, types)
  }

  /**
   * Returns a set of pairs obtained by `ps`, changing all the occurrences of `oldvar` into `newvar`.
   */
  private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }

  /**
   * Returns a set of pairs obtained by `ps`, removing those pairs which contains `v`.
   */
  private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }

  /**
   * Returns a set of pairs, obtained adding to `ps` those pairs `(l,r)` such that `(l,v)` and
   * `(v,r)` are in `ps`.
   */
  private def joinThrough(ps: Set[UP[Int]], v: Int) =
    (for (
      UP(l, r) <- ps; if l == v || r == v; first = if (l == v) r else l;
      UP(l1, r1) <- ps; if l1 == v || r1 == v; second = if (l1 == v) r1 else l1
    ) yield UP(first, second)) ++ ps

  /**
   * An object of pair sharing. Each object is composed of a set of unordered pair of variables (the variables
   * which may possibly share) and the types of those variables.
   * @param ps set of unordered pair of variables (the variables which may possibly share)
   * @param rtypes the sequence of types for the variable in ps, with the reverse ordering. `rtypes(0)` is
   * actually the type of the variables `rtypes.size - 1`. This is done for speeding execution.
   */
  case class Property private[PairSharingDomain] (val ps: Set[UP[Int]], val rtypes: Seq[om.Type]) extends ObjectProperty[Property] {

    type Domain = PairSharingDomain.this.type

    def domain = PairSharingDomain.this

    def dimension = rtypes.size

    def fiber = rtypes.reverse

    def top = domain.top(rtypes.reverse)

    def bottom = domain.bottom(rtypes.reverse)

    def isTop = this == top

    def isBottom = ps.isEmpty

    def isEmpty = false

    def union(that: Property) = {
      assert(dimension == that.dimension)
      new Property(ps union that.ps, rtypes)
    }

    def intersection(that: Property) = {
      assert(dimension == that.dimension)
      new Property(ps intersect that.ps, rtypes)
    }

    def widening(that: Property) = union(that)

    def narrowing(that: Property) = that

    def addUnknownVariable(t: om.Type) = {
      new Property(ps, t +: rtypes)
    }

    def addVariable(t: om.Type) = {
      val ps2 = for (UP(i, j) <- ps; if (i == j)) yield UP(i, dimension)
      new Property(ps ++ ps2, t +: rtypes)
    }

    def delVariable(v: Int) =
      if (v == dimension - 1)
        new Property(removeVariable(ps, v), rtypes drop 1)
      else
        new Property(removeVariable(ps, v) map { _.replace { x => if (x < v) x else x - 1 } }, (rtypes take (dimension - 1 - v)) ++ (rtypes drop (dimension - v)))

    def mapVariables(rho: Seq[Int]) = {
      val ps2 = for (UP(l, r) <- ps; if rho(l) != -1; if rho(r) != -1) yield UP(rho(l), rho(r))
      val rtypes2 = for { i <- rho; if i != -1 } yield rtypes(i)
      new Property(ps2, rtypes2)
    }

    /**
     * This method is similar to `connect`, but do not remove the common dimensions.
     */
    private[domains] def connectFull(that: Property, common: Int) = {
      assert(common <= dimension && common <= that.dimension)
      // index of the first common variable in the connected property
      val firstCommonInThis = dimension - common
      // remove all pairs in that involving a variable which is null in this. At the
      // same time, translate index
      val trimmedTranslatedThat = for {
        UP(l, r) <- that.ps
        if l >= common || !mustBeNull(l + firstCommonInThis)
        if r >= common || !mustBeNull(r + firstCommonInThis)
      } yield UP(l + firstCommonInThis, r + firstCommonInThis)
      // remove from this those pairs which only relates common variables. Moreover,
      // remove variables which are null in `that` (if a variable is null in that,
      // since it cannot be forced to be null due to call-by-value semantics, it had
      // to be null before.
      val trimmedThis = this.ps filter {
        case UP(l, r) => l < firstCommonInThis && (r < firstCommonInThis || !that.mustBeNull(r - firstCommonInThis))
      }
      // join one ps of this with one ps of that
      val j1 = for {
        UP(l, r) <- trimmedThis
        if r >= firstCommonInThis
        UP(l1, r1) <- trimmedTranslatedThat
        if r == l1
      } yield UP(l, r1)
      // join two ps of this
      val j2 = for (UP(l, r) <- trimmedThis; if r >= firstCommonInThis; UP(l1, r1) <- j1; if r == r1) yield UP(l, l1)
      new Property(trimmedThis ++ j1 ++ j2 ++ trimmedTranslatedThat, that.rtypes ++ rtypes.drop(common))
    }

    def connect(that: Property, common: Int) = {
      connectFull(that, common).delVariables(dimension - common until dimension)
    }

    def typeOf(v: Int, fs: Iterable[om.Field]) =
      if (fs.isEmpty) Some(rtypes(dimension - v - 1)) else if (mustBeNull(v)) None else
        Some(om.typeOf(fs.last))

    def addFreshVariable(t: om.Type) =
      if (om.mayShare(t, t)) new Property(ps + UP((dimension, dimension)), t +: rtypes) else new Property(ps, t +: rtypes)

    def assignNull(dst: Int = dimension - 1) = new Property(removeVariable(ps, dst), rtypes)

    def assignVariable(dst: Int, src: Int) = {
      val removed = removeVariable(ps, dst)
      if (mustBeNull(src))
        new Property(removed, rtypes)
      else
        new Property(removed ++ renameVariable(removed, dst, src) + UP(dst, src), rtypes)
    }

    def castVariable(v: Int, newtype: om.Type) = this

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field) = {
      if (mustBeNull(src)) // src is null, hence accessing its field returns an error
        bottom
      else {
        val removed = removeVariable(ps, dst)
        val renamed = renameVariable(removed, dst, src) filter { case UP(i, j) => om.mayShare(rtypes(dimension - 1 - i), rtypes(dimension - 1 - j)) }
        new Property(removed ++ renamed + UP(dst, src), rtypes)
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int) =
      if (mustBeNull(dst)) // src is null, hence accessing its field returns an error
        bottom
      else
        new Property(joinThrough(joinThrough(ps + UP(dst, src), src), dst), rtypes)

    def testNull(v: Int) = new Property(removeVariable(ps, v), rtypes)

    def testNotNull(v: Int) = if (mustBeNull(v)) bottom else this

    def mayBeNull(v: Int, fs: Iterable[om.Field]) = true

    def mustBeNull(v: Int, fs: Iterable[om.Field]) = !(ps contains UP(v, v))

    def mayShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) =
      (ps contains UP(v1, v2)) && om.mayShare(om.typeOf(fs1.last), om.typeOf(fs2.last))

    def mustShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = false

    def mayBeAliases(v1: Int, v2: Int) =
      (ps contains UP(v1, v2)) && om.mayBeAliases(rtypes(dimension - v1 - 1), rtypes(dimension - v2 - 2))

    def mustBeAliases(v1: Int, v2: Int) = false

    def mayBeWeakAliases(v1: Int, v2: Int) = true

    def mustBeWeakAliases(v1: Int, v2: Int) = mustBeNull(v1) && mustBeNull(v2)

    def mkString(vars: Seq[String]) = {
      val pairs = ps map { case UP(l, r) => s"(${vars(l)}, ${vars(r)})" }
      s"${pairs.mkString("[ ", ", ", " ]")} types ${rtypes.reverse.mkString("< ", ", ", " >")}"
    }

    override def toString = mkString(for (i <- 0 until dimension) yield i.toString)

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property =>
          if (rtypes == other.rtypes) {
            if (other.ps == ps)
              Some(0)
            else if (ps subsetOf other.ps)
              Some(-1)
            else if (other.ps subsetOf ps)
              Some(1)
            else
              None
          } else
            None
        case _ => None
      }
  }
}

/**
 * The companion object for `PairSharingDomain`, which is also a domain factory.
 */
object PairSharingDomain extends ObjectDomainFactory {

  private class PairSharingParser(val varNames: Seq[String]) extends RegexParsers {
    private val ident = """[@$a-zA-Z._][\w.]*""".r  // allow .$ in identifiers
    private val pair = ("(" ~> ident <~ ",") ~ ident <~ ")" ^^ { case x ~ y => new UP(varNames.indexOf(x), varNames.indexOf(y)) }
    private val sequence = "{" ~> repsep(pair, ",") <~ "}"
    def parseProperty(s: String) = parseAll(sequence, s)
  }

  def apply[OM <: ObjectModel](om: OM) = new PairSharingDomain(om)
}
