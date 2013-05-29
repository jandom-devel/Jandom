package javatest;

class A { }

class B { }

class ListA {
	ListA next;
	A v;
}

class Pair {
	A v;
	B w;
}

public class SimpleTest {
	int classX;
	int classY;
	int sum;

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

	static int parametric_static(int x, int y) {
		return x+y;
	}

	int parametric_dynamic(int x, int y) {
		classX = x;
		classY = y;
		sum = x + y;
		return sum;
	}

	@SuppressWarnings("unused")
	static void complexif() {
		int x = 0;
		int y = 0;
		int z = 0;
		if (2*x < y) z = 1;
	}

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

}
