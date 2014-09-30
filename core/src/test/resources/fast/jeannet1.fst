//! @tags fixpoint
//! @citations Jeannet_thesis:f2.1:p16

model jeannet_thesis_016 {
	
	var i, j;
	states k1, k2, k3;
	
	transition t1 := {
		from := k1;
		to := k2;
		guard := i + j <= 20;
		action := ;
	};
	
	transition t2 := {
		from := k1;
		to := k3;
		guard := i + j > 20;
		action := ;
	};
	
	transition t3 := {
		from := k2;
		to := k1;
		guard := i >= 10;
		action := i' = i + 4;
	};
	
	transition t4 := {
		from := k2;
		to := k1;
		guard := i < 10;
		action := i' = i + 2, j' = j + 1;
	};
	
}

strategy s {
	
	Region init := {state = k1 && i = 2 && j = 0};
	
	Region bad := {state = k2 && (i < 2j + 2 || 2j >= 10)};
	
}

