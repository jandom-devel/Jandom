package javatest;

class A1 {
}

class B1 {
}

class ListA1 {
	ListA1 next;
	A1 v;
}

class Pair1 {
	A1 v;
	B1 w;
}

public class SimpleTestBaf {
	int classX;
	int classY;
	int sum;

	static void sequential() {
		int x = 0;
		int y = 10;
		x = x + y;
	}

	@SuppressWarnings("unused")
	static void dup() {
		int x = 0;
		int y = x;
		int z = x+y;
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
		return x + y;
	}

	int parametric_dynamic(int x, int y) {
		classX = x;
		classY = y;
		sum = x + y;
		return sum;
	}

	static int parametric_caller(int x, int y) {
		x = 3;
		y = 4;
		int z = parametric_static(x, y);
		return z;
	}

	static int recursb(int x) {
		return recursa(x + 1);
	}

	static int recursa(int x) {
		if (x < 0)
			return recursb(x);
		else
			return x;
	}

	@SuppressWarnings("unused")
	static void mycast() {
		short s1 = 103;
		short s2 = 104;
		byte z = (byte) (s1 + s2);
		int i = s1 + s2;
		long l = s1 + s2;
		float f1 = s1;
		float f2 = i;
		double d = i;
	}

	@SuppressWarnings("unused")
	static void complexif() {
		int x = 0;
		int y = 0;
		int z = 0;
		if (2 * x < y)
			z = 1;
	}

	@SuppressWarnings("unused")
	static void objcreation() {
		A1 a1 = new A1();
		A1 a2 = new A1();
		ListA1 l = new ListA1();
		l.v = a1;
		l.next.next = l.next;
		a2 = l.v;
	}

	@SuppressWarnings("unused")
	static void classrefinement() {
		A1 a = new A1();
		B1 b = new B1();
		Pair1 p = new Pair1();
		p.w = b;
		a = p.v;
	}

	static Pair1 class_parametric(A1 a) {
		Pair1 p = new Pair1();
		p.v = a;
		a = new A1();
		return p;
	}

	static ListA1 pair_one(A1 a, B1 b) {
		ListA1 l = new ListA1();
		l.v = a;
		l.next = pair_one(a, b);
		return l;
	}

}
