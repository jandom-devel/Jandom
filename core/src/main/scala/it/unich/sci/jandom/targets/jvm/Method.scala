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

import it.unich.sci.jandom.targets.Target
import scala.collection.mutable.HashMap
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.Label
import org.objectweb.asm.tree.LabelNode
import org.objectweb.asm.tree.JumpInsnNode
import org.objectweb.asm.tree.LineNumberNode
import org.objectweb.asm.tree.FrameNode

/**
 * This class analyzes a single method of a class.
 * @author Gianluca Amato
 */

class Method(m: MethodNode) extends Target {  
  type ProgramPoint = Int
  type Annotation[Property] = HashMap[ProgramPoint,Property]
  type Tgt = Method
  
  private val startNode = createControlFlowGraph()
  
  /**
   * Build the control-flow graph of the method and returns the starting node.
   * It is called by the constructor.
   */
  private def createControlFlowGraph(): BasicBlock = {
    import scala.collection.JavaConversions._
    import AbstractInsnNode._    
    
    val iter = m.instructions.iterator.asInstanceOf[Iterator[AbstractInsnNode]]
    
    val labelBlocks = HashMap[LabelNode,BasicBlock]()
    val startBlock = new BasicBlock(m)
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
          if (! beginningOfBlock) {
            // since real instructions have been produced, we need to create a new block
            currentBlock.endNode = i.getPrevious
            val nextBlock = labelBlocks getOrElseUpdate(i, new BasicBlock(m))
            currentBlock.nextBlock = Some(nextBlock)
            currentBlock.jumpBlock = None
            currentBlock = nextBlock
            currentBlock.startNode = i
            beginningOfBlock = true
          } else {
            labelBlocks.getOrElseUpdate(i, currentBlock)
          }
        case i: JumpInsnNode =>                
          currentBlock.endNode = i          
          currentBlock.jumpBlock = Some(labelBlocks getOrElseUpdate(i.label, new BasicBlock(m)))
          if (i.getNext != null) {
            val nextBlock = new BasicBlock(m)            
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
    if (! beginningOfBlock) {
      currentBlock.endNode = m.instructions.getLast
      currentBlock.nextBlock = None
    }     
    startBlock
  }  
  
  def getAnnotation[Property]: Annotation[Property] = new Annotation[Property]
  def analyze(params: Parameters): Annotation[params.Property] = getAnnotation[params.Property]  
  def size = m.maxStack + m.maxLocals  
  override def toString = m.name
}
