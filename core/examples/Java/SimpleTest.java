import java.util.Random;
import java.util.*;

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

		static void inewarray(){
			int [] a = new int[8];
		}
		static void idup1(){
			double a=1.0;
			double b=a+a;
		}
		static void idup2(){
			double a=1.0;
			double b=1.0;
			double c=(a+a)+(b+b);
		}
		static void iadd() {
			int x = 4;
			int y = 3;
			x = x + y;
		}
		static void fuoriadd() {
			short x=9;
			short y=8;
			short z=(short)(x+y);
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
		static void idiv(){
			int x=15;
			int y=20;
			int z=x/y;
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
		static void pop(){
			Integer x = 5;
			Integer y=7;
			Integer z=x+y;
		}
		static void iswap(){
			int x=3;
			int y=4;
			int a=2;
			int z=((x+y)+a);
		}
		static void ifcmpeq(){
			int x=15;
			int y=0;
			if (x==5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifeq(){
			int x=15;
			int y=0;
			if (x==0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpne(){
			int x=15;
			int y=0;
			if (x!=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifne(){
			int x=15;
			int y=0;
			if (x!=0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmplt(){
			int x=15;
			int y=0;
			if (x<5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void iflt(){
			int x=15;
			int y=0;
			if (x<0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpgt(){
			int x=15;
			int y=0;
			if (x>5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifgt(){
			int x=15;
			int y=0;
			if (x>0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmple(){
			int x=15;
			int y=0;
			if (x<=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifle(){
			int x=15;
			int y=0;
			if (x<=0){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifcmpge(){
			int x=15;
			int y=0;
			if (x>=5){
				y=-1;
			}
			else {
				y=1;
			}
		}
		static void ifge(){
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
		static boolean bsearch (double k, double [] r){
			double lwb=1;
			double upb=100;
			int m;
			while (lwb<=upb){
				m=(int)((lwb+upb)/2); //divisione intera
				if (k==r[m]){
					return true;
				}
				else if (k<r[m]){
						upb=m-1;
					}
					else{
						lwb=m+1;
					}
				
			}
			return false;
		}
		static double [] bubblesort(double[] k){
			double b=100;
			double t, tmp;
			int j;
			while (b>=1){
				j=1;
				t=0;
				while (j<=(b-1)){
					if (k[j]>k[j+1]){
						tmp = k[j+1];
						k[j+1]=k[j];
						k[j]=tmp;
						t=j;
					}
					j=j+1;
				}
				if (t==0){
					return k;
				}
				b=t;
			}
			return k;
		}
		static double[] bubblesort(double[] k, int n){
			double b=n;
			double t, tmp;
			int j;
			while (b>=1){
				j=1;
				t=0;
				while (j<=(b-1)){
					if (k[j]>k[j+1]){
						tmp = k[j+1];
						k[j+1]=k[j];
						k[j]=tmp;
						t=j;
					}
					j=j+1;
				}
				if (t==0){
					return k;
				}
				b=t;
			}
			return k;
		}
		//da errore
		static void cousot78a(){
			double i=2;
			double j=0;
			while (true){
				if ((new Random()).nextBoolean()){
					i=i+4;
				}
				else {
					j=j+1;
					i=i+2;
				}
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
		static void octagon1(){
			int x=100;
			int y=0;
			while (x>=0){
				x=x-1;
				if ((new Random()).nextBoolean()){
					y=y+1;
				}
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
		static void octagon6(){
			
		}
		static void octagon6bis(){
			
		}
		
		//da errore
		static void parallellines(){
			int x=1;
			int y=1;
			while (true){
				x=x+y;
				y=2*y;
			}
		}
		static void policy1(){
			int i=0;
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
		//static void provatest CONTROLLARE provatest.R 
		//static void ratelim-monniaux() CONTROLLARE ratelim-monniaux.R
		static void reviewer3(){
			double x=0;
			double y=0;
			while(true){
				y=y+1;
				if ((new Random()).nextBoolean()){
					x=2*y;
				}
				else {
					x=-2*y;
				}
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
		static void widening (double [] k){
			int x=0;
			double y=0;
			while (x<100){
				x=x+1;
				if (k[x]<=0){
					y=x;
				}
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
}
