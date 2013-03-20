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

import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.AbstractInsnNode

/**
 * This class represents a basic block in the control-flow graph of a method.
 * @param m the MethodNode associated to this control block
 * @param start the index of the first instruction of the basic block
 * @param end the index of the last instruction of the basic block
 * @author Gianluca Amato
 */
class BasicBlock (val m: MethodNode) {
  var startNode: AbstractInsnNode = null
  var endNode: AbstractInsnNode = null
  var jumpBlock: Option[BasicBlock] = None
  var nextBlock: Option[BasicBlock] = None
  override def toString = m.name + " : " + startNode + " --> " + endNode + " : " + jumpBlock + " : " + nextBlock
}
