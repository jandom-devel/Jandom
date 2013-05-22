package javatest;

class A {
}

public class SimpleTest {
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

	@SuppressWarnings("unused")
	static void objcreation() {
		A a1 = new A();
		A a2 = new A();
	}

}
