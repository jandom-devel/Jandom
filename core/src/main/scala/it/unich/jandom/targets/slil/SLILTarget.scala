/**
  * Copyright 2013, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.targets.Target

/**
  * The abstract target class for SLIL. Each class in the slil package
  * which is a target should extend `SLILTarget`.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */

abstract class SLILTarget extends Target[SLILTarget] {
  /**
    * @inheritdoc
    * A program point for SLIL is a pair composed of a statement and a label. For
    * example, if `stmt` is a while statement, then `(stmt,head)` is the invariant of
    * the while.
    */
  type ProgramPoint = (SLILTarget, Any)
  type Tgt = SLILTarget
  type DomainBase = NumericalDomain
}
