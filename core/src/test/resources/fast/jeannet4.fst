//! @tags fixpoint
//! @citations Jeannet_thesis:p104

model jeannet_thesis_104 {
	
	var x, y, b0, b1;
	states k;
	
	transition t1 := {
		from := k;
		to := k;
		guard := x >= y && b0 = b1;
		action := b0' = 1 - b1, b1' = b0, x' = x + 1;
	};
	
	transition t2 := {
		from := k;
		to := k;
		guard := x >= y && b0 != b1;
		action := b0' = 1 - b1, b1' = b0, y' = y + 1;
	};
	
}

strategy s {
	
	Region init := {state = k && x = 0 && y = 0 && b0 = 0 && b1 = 0};
	
	Region bad := {state = k && x < y};
	
}

