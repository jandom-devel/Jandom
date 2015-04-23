package javatest;

class A {
}

class B {
}

interface ListInterface { }

interface ListInterface2 extends ListInterface { }

interface ListInterface3 extends ListInterface { }

interface ListInterface4 extends ListInterface3 { }

interface OtherInterface { }

interface InstantiableInterface { }

abstract class Abs implements ListInterface2 {
	int length;
}

abstract class Abs1 implements InstantiableInterface { }

class NoAbs1 extends Abs1 {
	int x;	
}

abstract class Abs2 extends NoAbs1 { }

class ListA implements ListInterface {
	ListA next;
	A v;
}

class ListA2 extends ListA {
	A w;
}

class ListA3 extends ListA implements ListInterface4, OtherInterface {
	A v2;	
}

class S1 {
}

class S2 extends S1 {
	A f1;
}

class S3 extends S2 {
	B f2;
}

class S5 extends S3 {
}

class S4 extends S2 {
	ListA l;
}

class R3 {
	S3 s;
}

class Pair {
	A v;
	B w;
}

class K {
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
