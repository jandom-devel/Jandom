package javatest;

public class ParameterTest {

	int emptyInt() {
		return 0;
	}
	
	ParameterTest emptyNull() {
		return null;
	}
	
	ParameterTest emptyThis() {
		return this;
	}

	int unusedParameters(int a, ParameterTest b, int c) {
		return 0;
	}
	ParameterTest returnParameter(int a, ParameterTest b) {
		return b;
	}
	
	ParameterTest returnThis(int a, ParameterTest b) {
		return this;
	}
	
	ParameterTest create() {
		ParameterTest a;
		a = new ParameterTest();
		return a;
	}

	ParameterTest create2(int x, ParameterTest y) {
		ParameterTest a;
		a = new ParameterTest();
		y = a;
		return y;
	}
	
	static int emptyIntS() {
		return 0;
	}
	
	static ParameterTest emptyNullS() {
		return null;
	}
	
	static int unusedParametersS(int a, ParameterTest b, int c) {
		return 0;
	}
	static ParameterTest returnParameterS(int a, ParameterTest b) {
		return b;
	}
}
