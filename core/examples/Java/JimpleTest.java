/**
 * Copyright 2013 Luca Mangifesta
 * Copyeight 2013, 2014 Gianluca Amato <gamato@unich.it> 
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

/**
 * This is a simple class for testing Jimple analyzer
 * 
 * @author Luca Mangifesta
 * @author Gianluca Amato <gamato@unich.it>
 */

public class JimpleTest {

	static int aa = 1;

	int yy = 4;

	static int align2grain(int i, int grain) {
		return ((i + grain - 1) & ~(grain - 1));
	}

	static void stringa() {
		String a = "ciao";
		String b = null;
	}

	static void isnotnull() {
		String h = "ciao";
		int y;
		if (h == null) {
			y = 0;
		} else {
			y = 1;
		}
	}

	static void isnull() {
		String h = null;
		int y;
		if (h != null) {
			y = 0;
		} else {
			y = 1;
		}
	}

	void putfield() {
		yy = 3;
	}

	void getfield(int i) {
		i = yy;
	}

	static void iadd() {
		long x = 35;
		long y = 45;
		long z = x + y;
		int x2 = 4;
		int y2 = 3;
		int z2 = x2 + y2;
		double x3 = 4.5;
		double y3 = 2.47;
		double z3 = x3 + y3;
		long h = z + 15;
	}

	static void idiv() {
		int x = 15;
		int y = 2;
		int z = x / y;
	}

	static void idup1() {
		int a = 1;
		int b = 2;
		int c = 3;
		int d = 4;
		int e = d + d;
	}

	static void getstaticfield() {
		int c = aa + 2;
	}

	static void putstaticfield() {
		aa = 4;
	}

	static void inewarray() {
		int[] a = new int[8];
	}

	static void isub() {
		int x = 315;
		int y = 284;
		int z = x - y;
	}

	static void imul() {
		int x = 15;
		int y = 20;
		int z = x * y;
	}

	static void ineg() {
		int x = 3;
		int y = -x;
	}

	static void irem() {
		int x = 5;
		int y = x % 2;
	}

	static void iinc() {
		int x = 1;
		while (x < 10) {
			x++;
		}
	}

	static void igoto() {
		int x = 3;
		while (x < 55) {
			x = x * 2;
		}
	}

	static int ireturn() {
		return 5;
	}

	static void ireturnvoid() {
		return;
	}

	static void iswap() {
		int x = 3;
		int y = 4;
		int a = 2;
		int z = ((x + y) + a);
	}

	static void ifcmpne() {
		int x = 15;
		int y = 0;
		if (x == 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifne() {
		int x = 15;
		int y = 0;
		if (x == 0) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifcmpeq() {
		int x = 15;
		int y = 0;
		if (x != 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifeq() {
		int x = 15;
		int y = 0;
		if (x != 0) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifcmpge() {
		int x = 15;
		int y = 0;
		if (x < 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifge() {
		int x = 15;
		int y = 0;
		if (x < 0) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifcmple() {
		int x = 15;
		int y = 0;
		if (x > 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifle() {
		int x = 15;
		int y = 0;
		if (x > 0) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifcmpgt() {
		int x = 15;
		int y = 0;
		if (x <= 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifgt() {
		int x = 15;
		int y = 0;
		if (x <= 0) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void ifcmplt() {
		int x = 15;
		int y = 0;
		if (x >= 5) {
			y = -1;
		} else {
			y = 1;
		}
	}

	static void iflt() {
		int x = 15;
		int y = 0;
		if (x >= 0) {
			y = -1;
		} else {
			y = 1;
		}
	}
}
