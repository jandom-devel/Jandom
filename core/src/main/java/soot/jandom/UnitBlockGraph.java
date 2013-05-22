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

/*
 * Derived from work by the Sable Research Group and others 1997-2003.
 * See the 'credits' file distributed with Soot for the complete list of
 * contributors.  (Soot is distributed at http://www.sable.mcgill.ca/soot)
 */

package soot.jandom;

import java.util.*;

import soot.*;
import soot.util.Chain;
import soot.toolkits.graph.*;

/**
 * Represents a {@link BlockGraph} where each {@link Unit} is a {@link Block} by
 * himself. It is useful for debugging.
 */

public class UnitBlockGraph extends BlockGraph {
	/**
	 * Constructs a {@link UnitBlockGraph} from a given {@link Body}.
	 *
	 * <p>
	 * Note that this constructor builds a {@link BriefUnitGraph} internally
	 * when splitting <tt>body</tt>'s {@link Unit}s into {@link Block}s. Callers
	 * who already have a {@link BriefUnitGraph} to hand can use the constructor
	 * taking a <tt>CompleteUnitGraph</tt> as a parameter, as a minor
	 * optimization.
	 *
	 * @param body
	 *            the {@link Body} for which to build a graph.
	 */
	public UnitBlockGraph(Body body) {
		this(new BriefUnitGraph(body));
	}

	/**
	 * Constructs a {@link BriefBigBlockGraph} representing the <tt>Unit</tt>
	 * -level control flow represented by the passed {@link BriefUnitGraph}.
	 *
	 * @param unitGraph
	 *            the {@link Body} for which to build a graph.
	 */
	public UnitBlockGraph(UnitGraph unitGraph) {
		super(unitGraph);
		soot.util.PhaseDumper.v().dumpGraph(this, mBody);
	}

	/**
	 * <p>
	 * Utility method for computing the basic block leaders for a {@link Body},
	 * given its {@link UnitGraph} (i.e., the instructions which begin new basic
	 * blocks).
	 * </p>
	 *
	 * <p>
	 * This implementation designates each unit as a block leader.
	 */

	@Override
	protected Set<Unit> computeLeaders(UnitGraph unitGraph) {
		Body body = unitGraph.getBody();
		if (body != mBody) {
			throw new RuntimeException(
					"BlockGraph.computeLeaders() called with a UnitGraph that doesn't match its mBody.");
		}
		Set<Unit> leaders = new HashSet<Unit>();
		Chain<Unit> units = body.getUnits();
		for (Iterator<Unit> unitIt = units.iterator(); unitIt.hasNext();) {
			Unit u = unitIt.next();
			leaders.add(u);
		}
		return leaders;
	}

}
