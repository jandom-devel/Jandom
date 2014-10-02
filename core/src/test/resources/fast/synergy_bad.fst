//! @tags termination
//! @citations GulavaniHKNR06:p9:f9

model gulavani_henzinger_kannan_nori_rajamani_sigsoft06_09 {

	var x, y;
	states q;

	transition t := {
		from := q;
		to := q;
		guard := y >= 0;
		action := y' = y + x;
	};

}

strategy s {
	
	Region init := {state = q && x = 0 && y = 0};

}

