/**
 * Copyright 2013 amato
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

package it.unich.sci.jandom.targets.slil

import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.targets.Target

/**
 * The abstract target class for SLIL. Each target in the slil package
 * which is a target should extend `SLILTarget`.
 * @author Gianluca Amato <gamato@unich.it>
 */

abstract class SLILTarget extends Target[SLILTarget] {
  type ProgramPoint = (SLILTarget, Int)
  type Tgt = SLILTarget
  type DomainBase = NumericalDomain
}
