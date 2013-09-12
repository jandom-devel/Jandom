 /*
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
*/
class SimpleTest {
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
	/////////////////////////////////////////////////////////
	
	//serve a testare il compare
	static void whileDouble() {
		double i = 0.0;
		while (i < 100.1) {
			i++;
		}
	}
	//serve a testare andInst e xorInst
	static int align2grain(int i, int grain) {
		return ((i + grain-1) & ~(grain-1));
	}
	//serve a testare pushInst(STRINGA) e pushInst(NULL)
	static void stringa (){
		String a="ciao";
		String b=null;
	}
	
	static void isnotnull (){
		String h="ciao";
		int y;
		if (h==null){
			y=0;
		}
		else {
			y=1;
		}
	}
	static void isnull (){
		String h=null;
		int y;
		if (h!=null){
			y=0;
		}
		else {
			y=1;
		}
	}
	int yy=4;
	
	void putfield() {
		yy = 3;
	}
	 void getfield(int i){
		i= yy;
	}
	
	
	static void iadd() {
		long x=35;
		long y=45;
		long z=x+y;
		int x2=4;
		int y2=3;
		int z2= x2+y2;
		double x3=4.5;
		double y3=2.47;
		double z3=x3+y3;
		long h=z+15;
	}
	static void idiv(){
		int x=15;
		int y=2;
		int z=x/y;
	}
	static void idup1(){
		int a=1;
		int b=2;
		int c=3;
		int d=4;
		int e=d+d;
	}
	static int aa=1;
	static void getstaticfield(){
		int c=aa+2;
	}
	static void putstaticfield(){
		aa=4;
	}
		static void provafor(){
			int x=5;
			for (int i=0; i<10; i++){
				x++;
			}
		}
		static void inewarray(){
			int [] a = new int[8];
		}
		
		
		static void isub(){
			int x=315;
			int y=284;
			int z=x-y;
		}
		static void imul(){
			int x=15;
			int y=20;
			int z=x*y;
		}
		
		static void ineg(){
			int x=3;
			int y=-x;
		}
		static void irem(){
			int x=5;
			int y=x%2;
		}
		static void iinc(){
			int x=1;
			while (x<10){
				x++;
			}
		}
		static void igoto(){
			int x=3;
			while(x<55){
				x=x*2;
			}
		}
		static int ireturn(){
			return 5;
		}
		static void ireturnvoid(){
			return;
		}
		static void iswap(){
			int x=3;
			int y=4;
			int a=2;
			int z=((x+y)+a);
		}
		static void ifcmpne(){
			int x=15;
			int y=0;
			if (x==5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifne(){
			int x=15;
			int y=0;
			if (x==0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpeq(){
			int x=15;
			int y=0;
			if (x!=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifeq(){
			int x=15;
			int y=0;
			if (x!=0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpge(){
			int x=15;
			int y=0;
			if (x<5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifge(){
			int x=15;
			int y=0;
			if (x<0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmple(){
			int x=15;
			int y=0;
			if (x>5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifle(){
			int x=15;
			int y=0;
			if (x>0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpgt(){
			int x=15;
			int y=0;
			if (x<=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifgt(){
			int x=15;
			int y=0;
			if (x<=0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmplt(){
			int x=15;
			int y=0;
			if (x>=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void iflt(){
			int x=15;
			int y=0;
			if (x>=0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		////////////////////////////////////
		
		static void arraylimit(int n){
			if (n<1){
				return;
			}
			int x=1;
			while (x<n){
				x=x+1;
			}
		}		
		static void doublerate(){
			int i=0;
			int j=0;
			while (i<=100){
				i=i+1;
				j=j+2;
			}
		}
		static void incr (){
			int i=1;
			while (i<10){
				i=i+1;
			}
		}
		static void increment(double k){
			if (k<0||k>1){
				return;
			}
			double i=1;
			double j=1;
			while(1==1){
				i=i+1;
				j=j+k;
				k=k-1;
			}
		}
		static void octagon3(double n){
			double x=0;
			if (n<0){
				return;
			}
			while (x<n){
				x=x+1;
			}	
		}
		static void octagon4(){
			int i=16;
			int x=1;
			while (i>0){
				x=x+1;
				i=i-1;
			}
		}
		
		static void octagon5(double x){
			if ((x<-100)||(x>100)){
				return;
			}
			double y=x;
			if (y<=0){
				y=-y;
			}
			if (y<=69){
				y=y;
			}
		}

		static void policy1(){
			int i=1;
			int j=10;
			while (j>=i){
				i=i+2;
				j=-1+j;
			}
		}
		static void policy2(){
			int i=0;
			int k=9;
			int j=-100;
			while (i<=100){
				i=i+1;
				while (j<=20){
					j=i+j;
				}
				k=4;
				while (k<=3){
					k=k+1;
				}
			}
		}
		static void preciseineq(){
			double x=4;
			double y=0;
			double z=0;
			if (x<4){
				y=1;
			}
			if (x!=4){
				z=1;
			}
		}
		static void seidl(){
			int i=0;
			int j;
			while (i<42){
				j=0;
				while(j<10){
					j=j+1;
				}
				i=i+j;
			}	
		}
		static void trydoublematrix(){
			int x=0;
			int y;
			while (x<100){
				x=x+1;
				y=0;
				while (y<x){
					y=y+1;
				}
			}
		}
		static void trygeneralptopes(){
			int x=1;
			int y=1;
			while (y<100){
				y=y+y;
				y=y+y;
				x=x+x;
				x=x+x;
			}
		}
		static void xyline(){
			double x=10;
			double y=-10;
			while (x>y){
				x=x-1;
				y=y+1;
			}
		}
		
		static void xyline1(double k){
			if (k<=0){
				return;
			}
			double x=k;
			double y=-k;
			while (x>y){
				x=x-1;
				y=y+1;
			}
		}
}