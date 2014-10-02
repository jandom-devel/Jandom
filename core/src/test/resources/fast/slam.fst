//! @tags termination
//! @citations GulavaniHKNR06:p5:f3 GulwaniJK09:f10d:p10 GulwaniZ10:f9:p12 HenzingerJMS02:f1:p2

model gulavani_henzinger_kannan_nori_rajamani_sigsoft06_05 {

	var lock_state, x, y;
	states q1, q2, q3, q4;

	transition t1 := {
		from := q1;
		to := q2;
		guard := true;
		action := lock_state' = 0, x' = y;
	};

	transition t2 := {
		from := q2;
		to := q3;
		guard := true;
		action := lock_state' = 1, y' = y + 1;
	};

	transition t3 := {
		from := q2;
		to := q3;
		guard := true;
		action := ;
	};

	transition t4 := {
		from := q3;
		to := q2;
		guard := x != y;
		action := ;
	};

	transition t5 := {
		from := q3;
		to := q4;
		guard := x = y;
		action := ;
	};

}

strategy s {
	
	Region init := {state = q1 && lock_state = 1};

	Region bad := {state = q4 && lock_state != 0};

}

