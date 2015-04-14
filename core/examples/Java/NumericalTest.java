/**
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
 * This is a simple class for testing Jandom numerical domains and both inter-
 * and intra-procedural analysis.
 * 
 * @author Gianluca Amato <gamato@unich.it>
 */
class NumericalTest {
	static void sequential() {
		int x = 0;
		int y = 10;
		x = x + y;
	}

	@SuppressWarnings("unused")
	static void conditional() {
		int x = 0;
		int y = 0;
		if (x <= 10)
			y = 1;
		else
			y = 2;
	}

	static void loop() {
		int x = 0;
		while (x < 10)
			x = x + 1;
	}
	
	static void nested() {
		int z = 0;
		for (int i = 0; i < 10; i++)
			for (int j = 0; j < i; j++)
				z = z + 1;
	}

	static void longassignment() {
		int z = 0;
		for (int i = 0; i < 10; i++)
			for (int j = 0; j < i; j++)
				z = z + i + 2 * j;
	}

	static void topologicalorder() {
		int z = 1;
		if (z != 1)
			z = 3;
		else
			z = 2;
		z = z + 1;
	}

	static void whileDouble() {
		double i = 0.0;
		while (i < 100.1) {
			i++;
		}
	}

	static void arraylimit(int n) {
		if (n < 1) {
			return;
		}
		int x = 1;
		while (x < n) {
			x = x + 1;
		}
	}

	static void doublerate() {
		int i = 0;
		int j = 0;
		while (i <= 100) {
			i = i + 1;
			j = j + 2;
		}
	}

	static void increment(double k) {
		if (k < 0 || k > 1) {
			return;
		}
		double i = 1;
		double j = 1;
		while (1 == 1) {
			i = i + 1;
			j = j + k;
			k = k - 1;
		}
	}

	static void octagon3(double n) {
		double x = 0;
		if (n < 0) {
			return;
		}
		while (x < n) {
			x = x + 1;
		}
	}

	static void octagon4() {
		int i = 16;
		int x = 1;
		while (i > 0) {
			x = x + 1;
			i = i - 1;
		}
	}

	static void octagon5(double x) {
		if ((x < -100) || (x > 100)) {
			return;
		}
		double y = x;
		if (y <= 0) {
			y = -y;
		}
		if (y <= 69) {
			y = y;
		}
	}

	static void policy1() {
		int i = 1;
		int j = 10;
		while (j >= i) {
			i = i + 2;
			j = -1 + j;
		}
	}

	static void policy2() {
		int i = 0;
		int k = 9;
		int j = -100;
		while (i <= 100) {
			i = i + 1;
			while (j <= 20) {
				j = i + j;
			}
			k = 4;
			while (k <= 3) {
				k = k + 1;
			}
		}
	}

	static void preciseineq() {
		double x = 4;
		double y = 0;
		double z = 0;
		if (x < 4) {
			y = 1;
		}
		if (x != 4) {
			z = 1;
		}
	}

	static void seidl() {
		int i = 0;
		int j;
		while (i < 42) {
			j = 0;
			while (j < 10) {
				j = j + 1;
			}
			i = i + j;
		}
	}

	static void trydoublematrix() {
		int x = 0;
		int y;
		while (x < 100) {
			x = x + 1;
			y = 0;
			while (y < x) {
				y = y + 1;
			}
		}
	}

	static void trygeneralptopes() {
		int x = 1;
		int y = 1;
		while (y < 100) {
			y = y + y;
			y = y + y;
			x = x + x;
			x = x + x;
		}
	}

	static void xyline() {
		double x = 10;
		double y = -10;
		while (x > y) {
			x = x - 1;
			y = y + 1;
		}
	}

	static void xyline1(double k) {
		if (k <= 0) {
			return;
		}
		double x = k;
		double y = -k;
		while (x > y) {
			x = x - 1;
			y = y + 1;
		}
	}
}