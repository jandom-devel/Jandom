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


class A {
}

class B {
}

class ListA {
	ListA next;
	A v;
}

class Pair {
	A v;
	B w;
}

public class ObjectTest {
	int classX;
	int classY;
	int sum;

	@SuppressWarnings("unused")
	static void objcreation() {
		A a1 = new A();
		A a2 = new A();
		ListA l = new ListA();
		l.v = a1;
		l.next.next = l.next;
		a2 = l.v;
	}

	@SuppressWarnings("unused")
	static void classrefinement() {
		A a = new A();
		B b = new B();
		Pair p = new Pair();
		p.w = b;
		a = p.v;
	}

	static Pair class_parametric(A a) {
		Pair p = new Pair();
		p.v = a;
		a = new A();
		return p;
	}

	static ListA pair_one(A a, B b) {
		ListA l = new ListA();
		l.v = a;
		l.next = pair_one(a, b);
		return l;
	}

}