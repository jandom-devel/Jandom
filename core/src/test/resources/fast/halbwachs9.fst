//! @tags fixpoint
//! @citations Halbwachs_thesis:p128

model halbwachs_thesis_60 {
	
	var i, j, k;
	states k1, k2, k3;
	
	transition t1 := {
		from := k1;
		to := k2;
		guard := i <= 100;
		action := k' = k + 1;
	};
	
	transition t2 := {
		from := k1;
		to := k3;
		guard := i > 100;
		action := ;
	};
	
	transition t3 := {
		from := k2;
		to := k1;
		guard := true;
		action := i' = i + 4;
	};
	
	transition t4 := {
		from := k2;
		to := k1;
		guard := true;
		action := i' = i + 2, j' = j + 1;
	};
	
}

strategy s {
	
	Region init := {state = k1 && i = 0 && j = 0 && k = 0};
	
	Region bad := {state = k1 && i + 2j != 4k};
	
}

