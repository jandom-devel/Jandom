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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.benchmarks

import java.io.File
import scala.collection.immutable.PagedSeq
import it.unich.jandom.parsers.FastParser
import scala.util.parsing.input.PagedSeqReader
import java.io.FileReader

/**
 * This trait loads and parser all models in Alice directory.
 * @author Gianluca Amato <gamato@unich.it>
 */

trait FASTLoader {
  /**
   * The directory from where benchmarks are loaded
   */
  val dir = new File(getClass.getResource("/fast/").toURI);
  
  /**
   * A sequence of Alice models.
   */
  val ltss = for (model <- dir.listFiles()) yield {
    val fr = new FileReader(model)
    val source = new PagedSeqReader(PagedSeq.fromReader(fr))
    val result = FastParser().parse(source)
    fr.close()
    result.get
  }
}
