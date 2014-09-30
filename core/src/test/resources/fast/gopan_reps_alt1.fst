//! @tags fixpoint
//! @citations GopanR06:f3a:p5 GopanR07:f2:p5

model gopan_cav06_05a {

	var x, y;
	states n1, n2, n4, n5, n6;

	transition t1 := {
		from := n1;
		to := n2;
		guard := x <= 50;
		action := ;
	};

	transition t2 := {
		from := n2;
		to := n4;
		guard := true;
		action := y' = y + 1;
	};

	transition t6 := {
		from := n4;
		to := n5;
		guard := y >= 0;
		action := ;
	};

	transition t7 := {
		from := n5;
		to := n6;
		guard := true;
		action := x' = x + 1;
	};

}

strategy s {

	Region init := {state = n1 && x = 0 && y = 0};

	Region bad := {state = n1 && (x != y || x < 0 || x > 51)};

}

