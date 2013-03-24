/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom.targets.jvm

import java.io.{ PrintWriter, StringWriter }
import scala.collection.mutable.HashMap
import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util._
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.domains.NumericalProperty
import scala.collection.mutable.Queue
import scala.collection.mutable.BitSet
import it.unich.sci.jandom.targets.Parameters

/**
 * This class analyzes a single method of a class.
 * @author Gianluca Amato
 */

class Method(val methodNode: MethodNode) extends Target {
  type ProgramPoint = BasicBlock
  type Annotation[Property] = HashMap[ProgramPoint, Property]
  type Tgt = Method
  type DomainBase = JVMEnvDomain
  
  val startBlock = createControlFlowGraph()
  determineWidening(startBlock)

  /**
   * This class represents a basic block in the control-flow graph of the method. Each basic
   * block is characterized by a `startNode` and `endNode`, which are pointers to the first and
   * last instruction of the basic block. Morever, there is a `nextBlock` pointer which points
   * to the next instruction block during normal execution, and `jumpBlock` which points to the
   * next block when a jump is perfomed. They are all defined as vars, since they are not known
   * in advance when the block is built.
   * @author Gianluca Amato
   */
  class BasicBlock {
    var startNode: AbstractInsnNode = null
    var endNode: AbstractInsnNode = null
    var jumpBlock: Option[BasicBlock] = None
    var nextBlock: Option[BasicBlock] = None
    var visited = false
    var widening = false
    /**
     * Returns the index of the starting instruction in the method
     */
    def startIndex = methodNode.instructions.indexOf(startNode)

    /**
     * Returns the index of the ending instruction in the method
     */
    def endIndex = methodNode.instructions.indexOf(endNode)

    override def toString = {
      val next = if (nextBlock.isEmpty) " --- " else nextBlock.get.hashCode.toString
      val jump = if (jumpBlock.isEmpty) " --- " else jumpBlock.get.hashCode.toString
      hashCode + ": from " + startIndex + " to " + endIndex + " next " + next + " jump " + jump + (if (widening) "(widening)" else "")
    }

    def analyze[Property <: NumericalProperty[Property]](state: JVMEnv[Property]): Seq[(BasicBlock, JVMEnv[Property])] = {
      import Opcodes._
      val s = state.clone
      var node = startNode
      var lastNode = endNode.getNext()
      var exits = Seq[(BasicBlock, JVMEnv[Property])]()
      while (node != lastNode) {
        println(methodNode.instructions.indexOf(node) + " : " + s)
        val op = node.getOpcode
        node match {
          case node: InsnNode =>
            op match {
              case ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 => s.ipush(op - ICONST_0)
              case IADD => s.iadd
              case RETURN =>
              case _ => throw UnsupportedByteCodeException(node)
            }
          case node: IntInsnNode =>
            op match {
              case BIPUSH =>
                s.ipush(node.operand)
              case _ => throw UnsupportedByteCodeException(node)
            }
          case node: VarInsnNode =>
            op match {
              case ISTORE => s.istore(node.`var`)
              case ILOAD => s.iload(node.`var`)
              case _ => throw UnsupportedByteCodeException(node)
            }
          case node: JumpInsnNode =>
            op match {
              case IF_ICMPGT => {
                val scopy = s.clone
                scopy.if_icmpgt
                exits :+= (jumpBlock.get, scopy)
                s.if_icmple
              }
              case GOTO => exits :+= (jumpBlock.get, s)
              case _ => throw UnsupportedByteCodeException(node)
            }
          case node: LabelNode =>
          case node: LineNumberNode =>
          case node: FrameNode =>
          case _ => throw UnsupportedByteCodeException(node)
        }
        node = node.getNext()
      }
      if (nextBlock.isDefined)
        exits :+= ((nextBlock.get, s))
      exits
    }
  }

  /**
   * This method visit the control-flow graph, starting from the given `BasicBlock`
   */
  def visit(start: BasicBlock)(f: BasicBlock => Unit) {
    f(start)
    start.visited = true
    start.jumpBlock match {
      case Some(b) => if (!b.visited) visit(b)(f)
      case _ =>
    }
    start.nextBlock match {
      case Some(b) => if (!b.visited) visit(b)(f)
      case _ =>
    }
  }

  def determineWidening(block: BasicBlock = startBlock, visited: BitSet = BitSet()) {
    visited.add(block.startIndex)
    block.nextBlock match {
      case Some(b) => if (!visited.contains(b.startIndex)) determineWidening(b, visited)
      case _ =>
    }
    block.jumpBlock match {
      case Some(b) => if (!visited.contains(b.startIndex)) determineWidening(b, visited) else b.widening = true
      case _ =>
    }
  }

  /**
   * Builds the control-flow graph of the method and returns the starting node.
   * It is called by the constructor.
   */
  private def createControlFlowGraph(): BasicBlock = {
    import scala.collection.JavaConversions._
    import AbstractInsnNode._

    val iter = methodNode.instructions.iterator.asInstanceOf[java.util.Iterator[AbstractInsnNode]]

    val labelBlocks = HashMap[LabelNode, BasicBlock]()
    val startBlock = new BasicBlock
    startBlock.startNode = iter.next
    var currentBlock = startBlock

    // is true if no real instructions has been assigned yet to the 
    // current block
    var beginningOfBlock = true

    while (iter.hasNext) {
      // get instruction
      val i = iter.next

      // check type of instructions
      i match {

        case i: LabelNode =>
          if (!beginningOfBlock) {
            // since real instructions have been produced, we need to create a new block
            currentBlock.endNode = i.getPrevious
            val nextBlock = labelBlocks getOrElseUpdate (i, new BasicBlock())
            currentBlock.nextBlock = Some(nextBlock)
            currentBlock.jumpBlock = None
            currentBlock = nextBlock
            currentBlock.startNode = i
            beginningOfBlock = true
          }
        case i: JumpInsnNode =>
          currentBlock.endNode = i
          currentBlock.jumpBlock = Some(labelBlocks getOrElseUpdate (i.label, new BasicBlock()))
          if (i.getNext != null) {
            val nextBlock = i.getNext match {
              case ln: LabelNode => labelBlocks.getOrElseUpdate(ln, new BasicBlock())
              case _ => new BasicBlock
            }
            currentBlock.nextBlock = Some(nextBlock)
            currentBlock = nextBlock
            currentBlock.startNode = i.getNext
            beginningOfBlock = true
          }
        case i: LineNumberNode =>
        case i: FrameNode =>
        case _ =>
          beginningOfBlock = false
      }
    }
    if (!beginningOfBlock) {
      currentBlock.endNode = methodNode.instructions.getLast
      currentBlock.nextBlock = None
    }
    startBlock
  }

  def getAnnotation[Property]: Annotation[Property] = new Annotation[Property]

  def analyze(params: Parameters): Annotation[params.Property] = {
    val ann = new Annotation[params.Property]()
    ann(startBlock) = params.domain(methodNode.maxLocals)
    val taskList = Queue[BasicBlock](startBlock)
    while (!taskList.isEmpty) {
      val b = taskList.dequeue()
      val result = b.analyze(ann(b))
      for ((block, state) <- result) {
        if (ann contains block) {
          val modified = if (block.widening)
            ann(block) widening state
          else
            ann(block) union state
          if (modified) taskList.enqueue(block)
        } else {
          ann(block) = state
          taskList.enqueue(block)
        }
      }
    }
    ann
  }

  def size = methodNode.maxStack + methodNode.maxLocals

  override def toString = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    val p = new Textifier(Opcodes.ASM4) {
      override def visitMethodEnd {
        print(pw)
      }
    }
    val tracer = new TraceMethodVisitor(p)
    methodNode.accept(tracer)
    pw.flush
    pw.close
    sw.getBuffer.toString
  }
}
