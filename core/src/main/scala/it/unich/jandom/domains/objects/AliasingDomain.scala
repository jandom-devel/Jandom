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

package it.unich.jandom.domains.objects

import scala.annotation._
import scala.annotation.elidable._

import it.unich.jandom.objectmodels.ObjectModel
import it.unich.jandom.utils.DisjointSets

/**
 * The domain for definite weak aliasing. Two identifiers are weak aliased if either they are
 * both null, or they point to the same location. This information is encoded in a graph called
 * aliasing graph.
 * @note this class mixes methods for the public interface of object domains with methods for low level
 * functionalities which should be factored out.
 * @author Gianluca Amato <gamato@unich.it>
 */
class AliasingDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  dom =>

  import AliasingDomain._

  /**
   * A Span is a set of labeled edges departing from the same source node,
   * represented as a map from labels to target node.
   */
  private[objects]type Span = Map[om.Field, Node]

  /**
   * An EdgeSet is a map from a node to its corresponding outer span.
   */
  private[objects]type EdgeSet = Map[Node, Span]

  /**
   * Returns a full span for a node of type `t`.
   */
  private[objects] def fullSpan(t: om.Type): Span = {
    (for { f <- om.fields(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } yield f -> Node())(collection.breakOut)
  }

  def top(types: Fiber) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(Node()) else None
    val edges: EdgeSet = (for { (t, Some(n)) <- types zip labels } yield n -> fullSpan(t))(collection.breakOut)
    new Property(labels, edges, types)
  }

  def bottom(types: Fiber) =
    new Property(Seq.fill(types.size)(None), Map.empty, types)

  /**
   * Builds an aliasing graph with given labels, edges and, types. Edges are given in the form
   * of an EdgeSet, and they are completed by turning undefined spans into empty spans.
   */
  private[objects] def apply(labels: Seq[Option[Node]], partialEdgeSet: EdgeSet, types: Seq[om.Type]): Property = {
    val edgeSet: EdgeSet = (
      for (Some(l) <- labels.distinct) yield if (partialEdgeSet.isDefinedAt(l))
        l -> partialEdgeSet(l)
      else
        l -> Map.empty[om.Field, Node])(collection.breakOut)
    new Property(labels, edgeSet, types)
  }

  /**
   * Builds an aliasing graph with given labels, edges and types. Edges are given as a sequence
   * of triples `(n1,f,n2)` where `n1` is the source node, `n2` is the target node and `f` the field name.
   */
  private[objects] def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val partialEdgeSet = edges.groupBy(_._1).
      mapValues(_.groupBy(_._2).
        mapValues(_.head._3))
    apply(labels, partialEdgeSet, types)
  }

  /**
   * Aliasing information is represented with an aliasing graph.
   * @param labels nodes associated to each variable
   * @param edges the edge set of the graph.
   * @param types declared types of variables.
   */
  class Property(private[objects] val labels: Seq[Option[Node]], val edges: EdgeSet, private[objects] val types: Seq[om.Type]) extends ObjectProperty[Property] {

    checkInvariant()

    /**
     * This method checks if an aliasing graph is well formed.
     */
    @elidable(ASSERTION)
    private def checkInvariant() {
      assume(labels.length == types.length, s"Field `labels` has length ${labels.length} while field `types` has length ${types.length} in ${this}")
      for (Some(n) <- labels) { assume(edges.isDefinedAt(n), s"First level node ${n} has no corresponding span") }
      for (n <- edges.keys) { assume(labels contains Some(n), s"A span exists for node ${n} which is not first level in ${this}") }
      for (n <- nodes) { assume(completeNodeType(n).isDefined, s"Node ${n} has no defined type in ${this}") }
      for (Some(n) <- labels.toSet; t = nodeType(n); (f, tgt) <- edges(n); fields = om.fields(t)) {
        assume(fields contains f, s"node ${n} contains an invalid field ${f} in ${this}")
      }
    }

    /**
     * Determines whether `n` is a 1^ level (root) node.
     */
    private[objects] def isFirstLevel(n: Node): Boolean = {
      labels contains Some(n)
    }

    /**
     * Returns the set of nodes reachable by first level nodes in `nodes`.
     */
    private[objects] def reachableNodesFrom(nodes: Node*): Set[Node] = {
      assume(nodes forall isFirstLevel)
      val s = collection.mutable.Set(nodes: _*)
      val q = collection.mutable.Queue(nodes: _*)
      while (!q.isEmpty) {
        val n = q.dequeue
        if (isFirstLevel(n)) {
          for ((f, tgt) <- edges(n); if !(s contains tgt)) {
            s += tgt
            q.enqueue(tgt)
          }
        }
      }
      s.toSet
    }

    /**
     * Remove nodes from the graph. The set `nodes` should be closed under edge reachability.
     */
    private[objects] def restrictNot(nodes: Set[Node]): Property = {
      val newlabels = labels map { _ flatMap { n => if (nodes contains n) None else Some(n) } }
      val newedges = (edges -- nodes) mapValues { span => span filterNot { case (f, n) => nodes contains n } }
      new Property(newlabels, newedges, types)
    }

    /**
     * Returns the set of nodes reachable from the set of of all fir level nodes without passing
     * through node `forbidden`.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def reachableNodesForbidden(forbidden: Node): Set[Node] = {
      assume(isFirstLevel(forbidden))
      val firstLevel: Set[Node] = (for (Some(n) <- labels; if n != forbidden) yield n)(collection.breakOut)
      val secondLevel = (firstLevel flatMap { edges(_).values }) - forbidden
      firstLevel ++ secondLevel
    }

    /**
     * Returns the set of all the nodes in the graph.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def nodes: Set[Node] = {
      val firstLevel: Set[Node] = (for (Some(n) <- labels) yield n)(collection.breakOut)
      val secondLevel = (firstLevel flatMap { edges(_).values })
      firstLevel ++ secondLevel
    }

    /**
     * Returns the node associated with a reachable identifier, or `None` otherwise.
     * We assume f is in the type of the identifier v
     */
    private[objects] def nodeOf(v: Int, f: om.Field): Option[Node] = {
      assume(om.pathExists(types(v), f))
      labels(v) flatMap { edges(_).get(f) }
    }

    /**
     * Returns the node associated with a field, or `None` otherwise.
     * We assume the input is well typed
     */
    private[objects] def nodeOf(v: Int, fs: Iterable[om.Field]): Option[Node] = {
      assume(om.pathExists(types(v), fs.toSeq: _*))
      val on = labels(v)
      if (on.isEmpty) None else nodeOf(on.get, fs)
    }

    /**
     * Follow the chain of fields starting from node `n` and returns the nodes we reach,
     * or `None` if some fields is not defined in the graph.
     */
    private[objects] def nodeOf(n: Node, fs: Iterable[om.Field]): Option[Node] = {
      if (fs.isEmpty)
        Some(n)
      else edges(n).get(fs.head) match {
        case None => None
        case Some(nnew) => nodeOf(nnew, fs.tail)
      }
    }

    /**
     * Returns the glb of all the types of variables pointing to 'n`. We assume `n` is a 1^ level node
     */
    private[objects] def nodeType(n: Node): om.Type = {
      assume(isFirstLevel(n))
      om.concreteApprox(for ((Some(`n`), t) <- labels zip types) yield t).get
    }

    /**
     * Returns a glb approximation of all reachable identifiers pointing to node `n`.
     */
    private[objects] def completeNodeType(n: Node): Option[om.Type] = {
      val firstLevels = for ((Some(`n`), t) <- labels zip types) yield t
      val secondLevels = for ((_, span) <- edges; (f, `n`) <- span) yield om.typeOf(f)
      om.concreteApprox(firstLevels ++ secondLevels)
    }

    /**
     * Returns the new set of edges of node `n` assuming its type becomes `t`. If `t`
     * is not a subtype of the current type of `n`, it does nothing.
     */
    private[objects] def expandSpan(n: Node, t: om.Type): Span = {
      assume(isFirstLevel(n))
      var span = edges(n)
      val nt = nodeType(n)
      if (om.lteq(t, nt))
        for (f <- om.fields(t) -- om.fields(nt)) span += f -> Node()
      span
    }

    /**
     * Returns the edges removed from node `n` when its type becomes `t`. We assume that
     * `t` is a supertype of the type of `n`
     */
    private[objects] def reduceSpan(n: Node, t: om.Type): (Span, Iterable[Node]) = {
      assume(isFirstLevel(n) && om.lteq(nodeType(n), t))
      val removedFields = om.fields(nodeType(n)) -- om.fields(t)
      val removedNodes = for { (f, dst) <- edges(n); if removedFields contains f } yield dst
      (edges(n) -- removedFields, removedNodes)
    }

    /**
     * Returns a new edge-set if variable `v` does not point to `labels(v)` anymore
     */
    private[objects] def reducedEdgeSet(v: Int): EdgeSet = {
      labels(v) match {
        case None => edges
        case on @ Some(n) => if (labels.count(_ == on) == 1) edges - n else edges
      }
    }

    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph, which should be on the same fiber of `this`
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    private[objects] def tryMorphism(other: Property): Option[(Int, Morphism)] = {
      assume(fiber == other.fiber)

      class MorphismBuilder {
        private var status: Option[Int] = Some(0)
        private var map1 = Map[Node, Option[Node]]()
        private var map2 = Map[Node, Option[Node]]()

        def direction = status

        def morphism: Option[Morphism] = status match {
          case None => None
          case Some(-1) => Some(map2)
          case _ => Some(map1)
        }

        def matchFields(on1: Option[Node], on2: Option[Node]): Unit = (on1, on2) match {
          case (Some(n1), Some(n2)) =>
            for ((f, n) <- edges(n1)) matchNode(Some(n), other.edges(n2).get(f))
            for ((f, n) <- other.edges(n2)) matchNode(edges(n1).get(f), Some(n))
          case (Some(n1), None) =>
            for ((f, n) <- edges(n1)) matchNode(Some(n), None)
          case (None, Some(n2)) =>
            for ((f, n) <- other.edges(n2)) matchNode(None, Some(n))
          case (None, None) =>
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Unit = (status, on1, on2) match {
          case (Some(0), Some(n1), Some(n2)) =>
            (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
              case (false, false) =>
                map1 += n1 -> on2
                map2 += n2 -> on1
              case (true, false) =>
                map2 += n2 -> on1
                status = Some(-1)
              case (false, true) =>
                map1 += n1 -> on2
                status = Some(1)
              case (true, true) =>
                status = if (map1(n1) == on2) status else None
            }
          case (Some(-1), Some(n1), Some(n2)) =>
            if (!(map2 isDefinedAt n2)) {
              map2 += n2 -> on1
            } else
              status = if (map2(n2) != on1) None else status
          case (Some(1), Some(n1), Some(n2)) =>
            if (!(map1 isDefinedAt n1)) {
              map1 += n1 -> on2
            } else
              status = if (map1(n1) != on2) None else status
          case (Some(1), None, Some(n2)) =>
            status = None
          case (Some(-1), Some(n1), None) =>
            status = None
          case (_, None, Some(n2)) =>
            if (!(map2 isDefinedAt n2)) {
              status = Some(-1)
              map2 += n2 -> None
            } else
              status = if (map2(n2).isDefined) None else status
          case (_, Some(n1), None) =>
            if (!(map1 isDefinedAt n1)) {
              status = Some(1)
              map1 += n1 -> None
            } else
              status = if (map1(n1).isDefined) None else status
          case _ =>
        }

        def matchLabels(l1: Seq[Option[Node]], l2: Seq[Option[Node]]): Unit = {
          for ((on1, on2) <- (labels zip other.labels)) {
            matchNode(on1, on2)
            if (status.isEmpty) return
            matchFields(on1, on2)
            if (status.isEmpty) return
          }
        }

      }

      val morphismBuilder = new MorphismBuilder()
      morphismBuilder.matchLabels(labels, other.labels)
      morphismBuilder.direction match {
        case None => None
        case Some(d) => Some((d, morphismBuilder.morphism.get))
      }
    }

    private[objects] def intersectionWithMorphisms(other: Property): (Property, Morphism, Morphism) = {
      val partition = DisjointSets.empty[(Boolean, Option[Node])]
      // the boolean value is used to differentiate between nodes coming from the two graphs

      def computePartition(on1: Option[Node], on2: Option[Node]): Unit = {
        if (!partition.inSamePartition((false, on1), (true, on2))) {
          partition.union((false, on1), (true, on2))
          (on1, on2) match {
            case (Some(n1), Some(n2)) if isFirstLevel(n1) && other.isFirstLevel(n2) =>
              for ((f, n) <- edges(n1)) computePartition(Some(n), other.edges(n2).get(f))
              for ((f, n) <- other.edges(n2)) computePartition(edges(n1).get(f), Some(n))
            case (Some(n1), None) if isFirstLevel(n1) =>
              for ((f, n) <- edges(n1)) computePartition(Some(n), None)
            case (None, Some(n2)) if other.isFirstLevel(n2) =>
              for ((f, n) <- other.edges(n2)) computePartition(None, Some(n))
            case (None, None) =>
              throw new IllegalStateException("We should never reach this state")
            case (_, _) =>
          }
        }
      }

      def partitionToMorphism(graph: Boolean, nullNode: (Boolean, Option[Node])): Morphism = { (n: Node) =>
        val repr = partition((graph, Some(n)))
        if (repr == nullNode) None else repr._2
      }

      partition.union((false, None), (true, None))
      for ((on1, on2) <- labels zip other.labels)
        computePartition(on1, on2)
      val nullNode = partition((false, None))
      val morph1 = partitionToMorphism(false, nullNode)
      val morph2 = partitionToMorphism(true, nullNode)
      val newGraph = applyMorphism(morph1)
      (newGraph, morph1, morph2)
    }

    private[objects] def unionWithMorphisms(other: Property): (Property, Morphism, Morphism) = {

      // maps pair of nodes to a single new node
      val nodemap = scala.collection.mutable.Map.empty[(Option[Node], Option[Node]), Node]

      var morph1 = Map.empty[Node, Option[Node]]
      var morph2 = Map.empty[Node, Option[Node]]
      var newedges = Map.empty[Node, Span]

      // get a new node for a pair of nodes, and app appropriate correspodence for morphisms
      def addNode(on1: Option[Node], on2: Option[Node]): Node = {
        assume(on1.isDefined || on2.isDefined)
        val newnode = nodemap.getOrElseUpdate((on1, on2), Node())
        morph1 += newnode -> on1
        morph2 += newnode -> on2
        newnode
      }

      // match two first level nodes
      def matchNode(on1: Option[Node], on2: Option[Node]): Option[Node] = {
        if (on1.isDefined || on2.isDefined) {
          val newnode = addNode(on1, on2)
          var newspan: Span = Map.empty
          if (on1.isDefined)
            for ((f, n) <- edges(on1.get)) newspan += f -> addNode(Some(n), on2 flatMap { other.edges(_).get(f) })
          if (on2.isDefined)
            for ((f, n) <- other.edges(on2.get)) newspan += f -> addNode(on1 flatMap { edges(_).get(f) }, Some(n))
          newedges += newnode -> newspan
          Some(newnode)
        } else
          None
      }

      val newlabels = for ((on1, on2) <- labels zip other.labels) yield matchNode(on1, on2)
      val unionGraph = new Property(newlabels, newedges, types)
      (unionGraph, morph1, morph2)
    }

    /**
     * Apply a morphism to a graph.
     * @param m the morphism to apply to the graph `this`
     */
    private[objects] def applyMorphism(m: Morphism): Property = {
      val newLabels = labels map { _ flatMap m }
      val newEdges = for {
        (src, span) <- edges
        newsrc <- m(src)
      } yield newsrc -> (for {
        (f, dst) <- span
        newdst <- m(dst)
      } yield f -> newdst)
      new Property(newLabels, newEdges, types)
    }

    type Domain = dom.type

    def domain = dom

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property =>
          assume(fiber == other.fiber)
          tryMorphism(other) map { _._1 }
        case _ => None
      }
    }

    def widening(that: Property): Property = this union that

    def narrowing(that: Property): Property = that

    def union(other: Property): Property = (this unionWithMorphisms other)._1

    def intersection(other: Property): Property = (this intersectionWithMorphisms other)._1

    def isEmpty: Boolean = false

    def fiber = types

    def dimension = types.length

    def addUnknownVariable(t: om.Type): Property = {
      new Property(labels :+ None, edges, types :+ t)
    }

    def addFreshVariable(t: om.Type): Property = {
      val n = Node()
      new Property(labels :+ Some(n), edges updated (n, fullSpan(t)), types :+ t)
    }

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }

    def delVariable(v: Int): Property = {
      // Remove spurious edges
      val newedges = reducedEdgeSet(v)
      new Property(labels patch (v, Nil, 1), newedges, types patch (v, Nil, 1))
    }

    def mapVariables(rho: Seq[Int]): Property = {
      val newsize = rho.count(_ != -1)
      val revrho = collection.mutable.Buffer.fill(newsize)(0)
      for ((newidx, oldidx) <- rho.zipWithIndex; if newidx != -1) revrho(newidx) = oldidx
      mapVariablesReverse(revrho)
    }

    /**
     * It works like mapVariables, but if `rho(i)=j` then variable `j` in the
     * original aliasing graph becomes variable `i`.
     */
    private def mapVariablesReverse(rho: Seq[Int]): Property = {
      val newlabels = rho map labels
      val newtypes = rho map types
      val newedges = edges filterKeys { newlabels contains Some(_) }
      new Property(newlabels, newedges, newtypes)
    }

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = {
      val (privateLabels, commonLabels) = labels.splitAt(labels.size - common)

      val newlabels = privateLabels ++ other.labels.drop(common)
      val newtypes = types.dropRight(common) ++ other.types.drop(common)

      val commonNodes: Set[Node] = (for (on <- commonLabels; n <- on) yield n) (collection.breakOut)
      val privateNodes: Set[Node] =  (for (on <- privateLabels; n <- on) yield n) (collection.breakOut)
      val reachableNodes = privateNodes filter { n => commonNodes exists { m => mayShareNodes(m,n)}}

      val newedges =
        for ((src, span) <- edges) yield src ->
          (if (reachableNodes contains src)
            fullSpan(nodeType(src))
          else
            span)
      new Property(
        newlabels,
        (newedges ++ other.edges).filterKeys { newlabels contains Some(_) },
        newtypes)
    }

    def mkString(vars: Seq[String]) = {
      val s1 = for { (Some(n), v) <- labels.zipWithIndex } yield {
        s"${vars(v)} : ${types(v)} -> n${n}"
      }
      val s2 = for { Some(n) <- labels.distinct; (f, ntgt) <- edges(n) } yield {
        s"n${n} -- ${f} --> n${ntgt}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Edges: ", ", ", "")
    }

    def typeOf(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs) flatMap completeNodeType

    def assignNull(dst: Int = dimension - 1): Property = {
      new Property(labels.updated(dst, None), reducedEdgeSet(dst), types)
    }

    def assignVariable(dst: Int, src: Int): Property = {
      if (dst == src)
        this
      else labels(src) match {
        case None =>
          assignNull(dst)
        case osrc @ Some(src) =>
          new Property(labels.updated(dst, osrc), reducedEdgeSet(dst), types)
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
      (labels(dst), labels(src)) match {
        case (None, _) =>
          bottom
        case (Some(dstNode), None) =>
          val newedges = for ((n, span) <- edges) yield {
            if (n == dstNode)
              n -> (span - field)
            else if (nodeMayBeAliases(dstNode, n) && (span contains field))
              // if n and dst may be aliases, then it is possible we are changing node n
              n -> span.updated(field, Node())
            else
              n -> span
          }
          new Property(labels, newedges, types)
        case (Some(dstNode), Some(srcNode)) =>
          val newedges = for ((n, span) <- edges) yield {
            if (n == dstNode)
              n -> edges(dstNode).updated(field, srcNode)
            else if (nodeMayBeAliases(dstNode, n) && (om.fields(nodeType(n)) contains field))
              // if n and dst may be aliases, then it is possible we are changing node n
              n -> span.updated(field, Node())
            else
              n -> span
          }
          new Property(labels, newedges, types)
      }
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (mustBeNull(src))
        bottom
      else {
        nodeOf(src, field) match {
          case None => assignNull(dst)
          case on @ Some(n) =>
            val newedges =
              if (on == labels(dst))
                edges
              else if (isFirstLevel(n))
                reducedEdgeSet(dst)
              else
                reducedEdgeSet(dst).updated(n, fullSpan(types(dst)))
            new Property(labels.updated(dst, on), newedges, types)
        }
      }
    }

    def castVariable(v: Int, newtype: om.Type): Property = {
      assume(om.lteq(newtype, types(v)))
      labels(v) match {
        case None =>
          new Property(labels, edges, types updated (v, newtype))
        case Some(n) =>
          val nodeType = completeNodeType(n).get
          if (om.lteq(nodeType, newtype) || om.lteq(newtype, nodeType)) {
            val newspan = expandSpan(n, newtype)
            new Property(labels, edges updated (n, newspan), types updated (v, newtype))
          } else {
            domain.bottom(types updated (v, newtype))
          }
      }
    }

    def testNull(v: Int): Property = {
      labels(v) match {
        case None => this
        case Some(nullnode) =>
          val nodes = reachableNodesFrom(nullnode)
          restrictNot(nodes)
      }
    }

    def testNotNull(v: Int): Property = labels(v) match {
      case None => bottom
      case _ => this
    }

    def mustBeNull(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs).isEmpty

    def mayBeNull(v: Int, fs: Iterable[om.Field]) = true

    def mustBeNotNull(v: Int, fs: Iterable[om.Field]) = false

    def mayBeNotNull(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs).isDefined

    def mayShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = mayBeNotNull(v1, fs1) && mayBeNotNull(v2, fs2)

    def mustShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = false

    def mayShareNodes(n1: Node, n2: Node) = om.mayShare(nodeType(n1), nodeType(n2))

    /**
     * Returns a set of 1st level nodes which may be aliases for the node n
     */
    def mayBeAliasesOf(n: Node): Seq[Node] = {
      assume(isFirstLevel(n))
      val t = completeNodeType(n).get
      for (Some(othern) <- labels.distinct; if othern != n && om.mayBeAliases(completeNodeType(othern).get, t)) yield othern
    }

    def mayBeAliases(v1: Int, v2: Int) = {
      val result = for (n1 <- labels(v1); n2 <- labels(v2)) yield nodeMayBeAliases(n1, n2)
      result getOrElse false
    }

    private[objects] def nodeMayBeAliases(n1: Node, n2: Node) = om.mayBeAliases(completeNodeType(n1).get, completeNodeType(n2).get)

    def mustBeAliases(v1: Int, v2: Int) = false

    def mayBeWeakAliases(v1: Int, v2: Int) = true

    def mustBeWeakAliases(v1: Int, v2: Int) = labels(v1) == labels(v2)

    def nodesMayBeAliases(n1: Node, n2: Node) = om.mayBeAliases(completeNodeType(n1).get, completeNodeType(n2).get)
  }
}

object AliasingDomain extends ObjectDomainFactory {

  /**
   * A node in an aliasing graph. The current implementation use integers to represent
   * nodes, although boxing will probably eat all the performance benefits we could
   * gain from this choice. Nodes are significant only within the same graph.
   */
  class Node private (val i: Integer) extends AnyVal {
    override def toString() = i.toString
  }

  /**
   * The companion object for nodes. It only has an apply method which creates new fresh node.
   */
  object Node {
    /**
     * An internal counter used to create fresh nodes
     */
    private var current: Int = 0

    /**
     * Returns a fresh node, i.e., a node which is guaranteed to be different from all the
     * other nodes.
     */
    def apply(): Node = {
      if (current == Integer.MAX_VALUE) throw new IllegalStateException("Too big nome number reached.")
      current += 1
      new Node(current)
    }
  }

  /**
   * A morphism is a function from Node to Option[Node]. We could use
   * partial function, but I think a standard function is more convenient.
   */
  type Morphism = Function[Node, Option[Node]]

  def apply[OM <: ObjectModel](om: OM) = new AliasingDomain(om)
}
