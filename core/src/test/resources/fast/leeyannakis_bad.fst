//! @tags termination
//! @citations GulavaniHKNR06:p7:f6

model gulavani_henzinger_kannan_nori_rajamani_sigsoft06_07 {

	var y;
	states q;

	transition t := {
		from := q;
		to := q;
		guard := y > 0;
		action := y' = y - 1;
	};

}

strategy s {
	
	Region init := {state = q};

}

